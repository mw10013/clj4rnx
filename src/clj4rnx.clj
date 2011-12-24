(ns clj4rnx
  "
  GlobalOscActions.lua -  evaluate_env.clj4rnx = {}
"
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.contrib.math :as math]
   [clojure.contrib.logging :as log]
   [osc :as osc]))

(def *lpb* 4)

(osc/osc-debug true)
(defonce *client* (osc/osc-client "127.0.0.1" 8000))

(def *query-port* 3456)
(defonce *query-ctx* (let [result (atom [])
                           result-server (osc/osc-server *query-port*)]
                       (osc/osc-handle result-server "/clj4rnx/result/begin" (fn [msg] (reset! result [])))
                       (osc/osc-handle result-server "/clj4rnx/result"
                                       (fn [msg] (def msg* msg) (swap! result conj (-> msg :args first))))
                       {:result-server result-server :result result}))

(defn set-log-level!
  "http://www.paullegato.com/blog/setting-clojure-log-level/"
  [level]
  "Sets the root logger's level, and the level of all of its Handlers, to level."
  (let [logger (log/impl-get-log "")]
    (.setLevel logger level)
    (doseq [handler (.getHandlers logger)]
      (. handler setLevel level))))

(set-log-level! java.util.logging.Level/FINEST)

(defn ev [& args] (osc/osc-send *client* "/renoise/evaluate" (apply str args)))

(defn bootstrap []
  (ev "
if clj4rnx.client == nil then
  local client, socket_error = renoise.Socket.create_client(\"localhost\", " *query-port* ", renoise.Socket.PROTOCOL_UDP)
  if (socket_error) then 
    renoise.app():show_warning((\"Failed to start the OSC client. Error: '%s'\"):format(socket_error))
    return
  end
  clj4rnx.client = client
end
")
  (ev "
    clj4rnx.send_result = function(s) clj4rnx.client:send(renoise.Osc.Message('/clj4rnx/result', {{tag='s', value=s}})) end
"))

(defn query
  [& args]
  (ev "clj4rnx.client:send(renoise.Osc.Message(\"/clj4rnx/result/begin\"))")
  (apply ev args)
  (ev "clj4rnx.client:send(renoise.Osc.Message(\"/clj4rnx/result/end\"))")
  (if (osc/osc-recv (:result-server *query-ctx*) "/clj4rnx/result/end" 2000)
    (let [s (apply str @(:result *query-ctx*))]
      (if (seq s) (read-string s)))
    (throw (Exception. "Timed out waiting for query result."))))

(defn- query-tracks-raw []
  (query "
do
  clj4rnx.send_result('[')
  for i, track in ipairs(renoise.song().tracks) do
    clj4rnx.send_result('{:name \"' .. track.name .. '\" :track-index ' .. i)
    for pos, col in renoise.song().pattern_iterator:note_columns_in_track(i, false) do
      if col.instrument_value ~= 255 then
        clj4rnx.send_result(' :instr-index ' .. col.instrument_value)  
        break
      end
    end
    clj4rnx.send_result('}')
  end
  clj4rnx.send_result(']')  
end"))

(defn query-tracks []
  (->> (query-tracks-raw)
       (map #(-> %
                 (update-in [:track-index] dec)
                 (update-in [:instr-index] (fnil identity (-> % :track-index dec)))
                 (assoc :track-key (-> % :name keyword))))
       (map #(vector (:track-key %) %))
       (into {})))

; (query-tracks-raw)
; (query-tracks)

(defn query-patr [index-or-name]
  (let [patr-snippet (if (number? index-or-name)
                       (str "
  local patr = renoise.song():pattern(" (inc index-or-name) ")")
                       (str "
  local patr = nil
  for i, o in ipairs(renoise.song().patterns) do
    if o.name == '" (name index-or-name) "' then
      patr = o
      break
    end
  end"))]
    (query "
do
" patr-snippet "
  clj4rnx.send_result('{:name \"' .. patr.name .. '\" :number-of-lines ' .. patr.number_of_lines .. ' :patr-tracks [')
  for i, track in ipairs(patr.tracks) do
    clj4rnx.send_result('{:lines [')
    for ii, line in ipairs(track.lines) do
      if not line.is_empty then
        clj4rnx.send_result('{:index ' .. (ii - 1) .. ' :note-cols [')
        for iii, note in ipairs(line.note_columns) do
          clj4rnx.send_result('{:note ' .. note.note_value .. ' :vol ' .. note.volume_value .. '}')
        end  
        clj4rnx.send_result(']}')
      end
    end 
    clj4rnx.send_result(']}')
  end
  clj4rnx.send_result(']}')
end")))

; (pr (query-patr 0))
; (pr (query-patr "grv-1"))
; (pr (query-patr :grv-1))
; (def rnx-patr* (query-patr 0))
; (query-patr 1)

; renoise.song().patterns[].tracks[].lines[].note_columns[].note_value
;  -> [number, 0-119, 120=Off, 121=Empty]

(defn- finish-pending-noteoffs [number-of-lines {:keys [notes pending]}]
  (let [noteoff-t (/ number-of-lines *lpb* 4)]
    (->> pending
         (remove nil?)
         (reduce (fn [notes index]
                   (let [{:keys [t] :as n} (notes index)]
                     (assoc notes index (assoc n :d (- noteoff-t t)))))
                 notes))))

(defn- cook-patr-track [number-of-lines{:keys [lines] :as  patr-track}]
  (let [notes (->> lines
                   (reduce (fn [{:keys [notes pending] :as ctx} {:keys [index note-cols]}]
                             (let [line-t (/ index *lpb* 4)]
                               (->> note-cols
                                    (remove (fn [col] (= (:note col) 121)))
                                    (map-indexed vector)
                                    (reduce (fn [{:keys [notes pending] :as ctx} [note-col-index {:keys [note vol]}]]
                                              (let [ctx (if-let [pending-index (pending note-col-index)]
                                                          (let [{:keys [t] :as  n} (notes pending-index)]
                                                            (-> ctx
                                                                (update-in [:pending] assoc note-col-index nil)
                                                                (update-in [:notes] assoc pending-index
                                                                           (assoc n :d (- line-t t)))
                                                                (assoc :pending (assoc pending note-col-index nil))))
                                                          ctx)]
                                                (if (= note 120)
                                                  ctx
                                                  (-> ctx
                                                      (update-in [:pending] assoc note-col-index (count notes))
                                                      (update-in [:notes] conj {:t line-t :note note :v (/ vol 254)})))))
                                            ctx))))
                           ; max number of notes is 12.
                           {:notes [] :pending (vec (repeat 12 nil))})
                   (finish-pending-noteoffs number-of-lines))]
    ; clear out pending
    (assoc patr-track :notes (log/spy notes))))

; (pr (cook-patr rnx-patr*))

(defn cook-patr [{:keys [number-of-lines] :as patr}]
  (update-in patr [:patr-tracks] #(->> % (map (partial cook-patr-track number-of-lines)) vec)))

(defn cached-patr [index-or-name])

; rprint(renoise.song():instrument(1).plugin_properties.available_plugins)
; (ev "print(renoise.song():instrument(1).plugin_properties.plugin_name)")
; (ev "print(renoise.song():instrument(1).plugin_properties.plugin_device.active_preset)")
; rprint(renoise.song():instrument(1).plugin_properties.plugin_device.presets)
; Audio/Generators/VST/Nexus
; Audio/Generators/VST/Sylenth1

; rprint(renoise.song().tracks[1].available_devices)
; Rprint(renoise.song().tracks[1].devices)
; renoise.song().tracks[]:insert_device_at(device_name, device_index)
; renoise.song().tracks[]:device(index)
; renoise.song().tracks[].devices[].parameters[]
; renoise.song().tracks[].devices[].parameters[].name
; renoise.song().tracks[].prefx_volume
; renoise.song().tracks[].postfx_volume
; renoise.song().patterns[].tracks[]:find_automation(parameter)
; renoise.song().patterns[].tracks[]:create_automation(parameter)
; renoise.song().patterns[].tracks[]:delete_automation(parameter)
; renoise.song().patterns[].tracks[].automation[].playmode
; renoise.song().patterns[1].tracks[1].automation[1].playmode = renoise.PatternTrackAutomation.PLAYMODE_POINTS
; renoise.song().patterns[1].tracks[1].automation[1].playmode = renoise.PatternTrackAutomation.PLAYMODE_LINEAR
; renoise.song().patterns[1].tracks[1].automation[1].playmode = renoise.PatternTrackAutomation.PLAYMODE_CUBIC
; renoise.song().patterns[].tracks[].automation[].points
; rprint(renoise.song().patterns[1].tracks[1].automation[1].points)
; renoise.song().patterns[1].tracks[1].automation[1].points = {{time=1, value=0.5},{time=5.5, value=1},{time=8, value=0}}
; renoise.song().patterns[1].tracks[1].automation[1].points = {{time = 1, value = 1}, {time = 5, value = 0}}

(defn reset-song [song]
  (ev "clj4rnx.song = renoise.song()")
  (ev "clj4rnx.song.transport.bpm = " (:bpm song))
  (ev "clj4rnx.song.transport.lpb = " *lpb*)
  (ev "clj4rnx.sequencer = clj4rnx.song.sequencer")

  (ev "
do local patr_index = #renoise.song().patterns + 1
  while patr_index <= " (-> song :patrs count) " do
    renoise.song().sequencer:insert_sequence_at(patr_index, patr_index)
    patr_index = patr_index + 1
  end
end"))

(defn- note-col [t off-t offs]
  (let [index (or  (->> offs
                        (map-indexed vector)
                        (filter (fn [[index off-t]] (when (>= t off-t) index)))
                        ffirst)
                   (count offs))]
    [index (assoc offs index off-t)]))

(defn set-notes [{:keys [track-index instr-index] :as track} pitch-f offs notes]
  (ev "clj4rnx.track = clj4rnx.song:track(" (inc track-index) ")")
  (ev "clj4rnx.pattern_track = clj4rnx.pattern:track(" (inc track-index) ")")
  (ev "
for i,v in ipairs(clj4rnx.pattern_track.lines) do
  v:clear()
end")
  (dorun (map-indexed
          (fn [index off-t]
            (ev "clj4rnx.note_column = clj4rnx.pattern_track:line(" (inc (int (* off-t 4 *lpb*))) "):note_column(" (inc index) ")")
            (ev "clj4rnx.note_column.note_string = 'OFF'" ))
          offs))
  (let [offs (reduce (fn [offs {:keys [t deg oct acc v d a] :as e :or {oct 0 v 3/4 acc 0 a 1}}]
                          (let [l (inc (int (* t 4 *lpb*)))
                                [note-col offs] (note-col t (+ t (* d a)) offs)
                                note-col (inc note-col)]
                            (ev "clj4rnx.note_column = clj4rnx.pattern_track:line(" l "):note_column(" note-col ")")
                            (ev "clj4rnx.note_column.note_value = " (pitch-f deg oct acc))
                            (ev "clj4rnx.note_column.volume_value = " (math/round (* v 127)))
                            (ev "clj4rnx.note_column.instrument_value = " instr-index)
                            (ev "clj4rnx.note_column = clj4rnx.pattern_track:line(" (int (+  l (* d 4 *lpb* a))) "):note_column(" note-col ")")
                            (ev "clj4rnx.note_column.note_string = 'OFF'" )
                            offs))
                          (or offs []) notes)
        vis-note-cols (count offs)]
      (ev "if clj4rnx.track.visible_note_columns < " vis-note-cols
          " then clj4rnx.track.visible_note_columns = " vis-note-cols " end")
      offs))

(defn set-alias [alias-patr-index _ {:keys [track-index]}]
  (ev "clj4rnx.pattern:track(" (inc track-index) ").alias_pattern_index = " (inc alias-patr-index))
  nil)

(def e-re* #"(([whqest\.]+)|(-?[\d/\.]+)([#b]+)?([+-]+)?([whqest\.]+)?([\d/\.]+)?\s*(\{.*})?)")
; (re-find e-re* "-1-")
; (re-find e-re* "1#+hq")
; (map #(re-find e-re* %) ["1bbet {:b :abacab}" "1#0.5{:a 2}" "1b""1/4" "0.5" "1" "2+" "7--" "e"])

(defn- parse-e [s]
  (when-not (str/blank? s)
    (if-let [[_ _ r deg acc oct d v m] (re-find e-re* s)]
      (let [d (or r d)
            acc (reduce #(+ %1 ({\# 1 \b -1} %2)) 0 acc)
            oct (reduce #(+ %1 (if (= %2 \+) 1 -1)) 0 oct)
            d-map {\w 1 \h 1/2 \q 1/4 \e 1/8 \s 1/16}
            d (if-not d 1/4 (first (reduce (fn [[d last] ch]
                                             (if-let [n (d-map ch)]
                                               [(+ d n) n]
                                               (condp = ch
                                                   \. [(+ d (/ last 2)) (+ last (/ last 2))]
                                                   \t [(+ (- d last) (* 2/3 last)) (* 2/3 last)]))) [0 0] d)))]
        (merge
         (conj {:d d}
               (when deg [:deg (read-string deg)])
               (when-not (zero? acc) [:acc acc])
               (when-not (zero? oct) [:oct oct])
               (when v [:v (read-string (str \0 v))]))
         (if m (read-string m))))
      (throw (IllegalArgumentException. (str "parse-e: unable to parse " s))))))

(defn- to-e [x]
  (cond
   (string? x) (parse-e x)
   (vector? x) (->> x (map to-e) vec)
   (seq? x) (map to-e x)
   :else (throw (IllegalArgumentException. (str "to-e: unexpected arg: " x)))))

(defn- to-es
  ([x] (to-es {:t 0 :es []} x))
  ([ctx x]
     (cond
      (map? x) (conj ctx [:t (+ (:t ctx) (:d x))] (when (:deg x) [:es (conj (:es ctx) (assoc x :t (:t ctx)))]))
      (vector? x) (reduce (fn [ctx x] (to-es ctx x)) ctx x)
      (seq? x) (let [new-ctx (reduce (fn [{max-t :max-t :as new-ctx} val]
                                       (let [new-ctx (to-es (assoc new-ctx :t (:t ctx)) val)]
                                              (assoc new-ctx :max-t (max (:t new-ctx) (:max-t new-ctx) max-t))))
                                          (assoc ctx :es [] :max-t (:t ctx)) x)]
                 (assoc new-ctx :t (:max-t new-ctx) :es (vec (concat (:es ctx) (sort #(compare (:t %1) (:t %2)) (:es new-ctx)))))))))

(defn- es-to-bars
  ([es] (es-to-bars nil es))
  ([bar-cnt es]
     (let [bars (reduce (fn [bars {t :t :as e}]
                          (let [index (int t)
                                bars (into bars (repeat (- (inc index) (count bars)) []))]
                            (assoc bars index (conj (bars index) (assoc e :t (- (e :t) index)))))) [] es)]
       (if bar-cnt (take bar-cnt (concat bars (repeat []))) bars ))))

(defn- normalize [s] (-> s (str/replace e-re* #(if-not (-> 0 % empty?) (str \" (% 0) \") ""))
                         (str/replace "'(" "(list ")
                         ((partial str \[) \])))

(defn- parse
  ([s] (parse nil s))
  ([bar-cnt s] (->> s normalize read-string eval to-e to-es :es (es-to-bars bar-cnt) repeat (mapcat identity))))

; (take 2 (parse "'(4 3 2 1)"))

(def patr-merge (partial merge-with (fn [val-in-result val-in-latter]
                                      (if (fn? val-in-latter)
                                        (if (fn? val-in-result) (comp val-in-latter val-in-result) (val-in-latter val-in-result))
                                        val-in-latter))))

(defn- step-all [step buf] (let [{:keys [t d]} (first step)] (map #(assoc % :t t :d d) buf)))

; (step-all [{:t 1/4 :d 1/4}] '({:t 0, :deg 1, :d 1/2} {:t 0, :deg 5, :d 1/2}))

(defn- step-index
  ([step buf] (step-index nil step buf))
  ([comp step buf] (let [buf (if comp (sort-by #(+ (* (:oct % 0) 12) (:deg %)) comp buf) buf)]
                     (reduce (fn [coll {:keys [deg] :as index}]
                               (if-let [n (nth buf (if (pos? deg) (dec deg) (+ (count buf) deg)) nil)]
                                 (conj coll (conj n [:t (:t index)] [:d (:d index)]
                                                  #_(if (:oct index) [:oct (:oct index)])
                                                  (if (:oct index) [:oct (+ (:oct n 0) (:oct index))])))
                                 coll)) [] step))))

; (take 2 (step-seq step-index(parse "1e 2e 2-e 1") (parse "'(1h 5h)")))
; (step-index [{:t 1/2 :deg 2 :d 1/4}] [{:t 0 :deg 2 :d 1} {:t 0 :deg 22 :d 1}])
; (take 1 (step-seq step-index (parse "1e 2e 1e") (parse "'(1h 3h 5h)")))

(defn- step-seq
  ([] (take 3 (step-seq (parse "1") (parse "1ww"))))
  ([steps bars] (comment (log/debug (apply str (repeat 20 \*)))) (step-seq step-all steps bars))
  ([f steps bars] (step-seq {} f steps bars))
  ([ctx f steps bars]
     (lazy-seq 
      (when (and (seq steps) (seq bars))
        (let [step-notes (->> steps first (group-by :t) (into (sorted-map)) (map second))
              {:keys [b ctx-t] :as ctx} (reduce
                                         (fn [{:keys [ctx-t src-notes buf b] :as ctx} step]
                                           (let [{:keys [t d]} (first step)
                                                 [new-notes src-notes] (split-with #(<= (:t %) t) src-notes)
                                                 buf (->> (concat (map #(update-in % [:d] - (- t ctx-t)) buf)
                                                                  (map (fn [{tt :t :as n}] (update-in n [:d] - (- t tt))) new-notes))
                                                          (filter #(> (:d %) 0)))
                                                 ctx (assoc ctx :ctx-t t :buf buf :src-notes src-notes)]
                                             (conj ctx (when (seq buf) [:b (vec (concat (:b ctx) (vec (f step buf))))]))))
                                         (assoc ctx :b [] :ctx-t 0 :src-notes (first bars))
                                         step-notes)
              ctx (update-in ctx [:buf] (fn [buf] (->> buf (map (fn [{t :t :as n}] (update-in n [:d] - (- 1 ctx-t))))
                                                      (filter #(> (:d %) 0)))))]
          (cons b (step-seq ctx f (rest steps) (rest bars))))))))

(defn- deg-to-pitch
  ([deg oct acc] (deg-to-pitch 48 deg oct acc))
  ([base deg oct acc] (+ base ([0 2 4 5 7 9 11] (dec deg)) (* oct 12) acc)))

(defn set-note-bars [{:keys [note-f] :as track} pitch-f offs bars]
  (let [offs (set-notes track pitch-f offs
                           (map (or note-f identity)
                                (mapcat (fn [index bar] (map #(update-in % [:t] (partial + index)) bar))
                                        (iterate inc 0) bars)))
        bars-t (count bars)]
    (->> offs (map #(- % bars-t)) (remove neg?) vec)))

(defn set-auto-bars [{:keys [device-index param-index playmode] :as auto :or {playmode "PLAYMODE_LINEAR"}} bars]
  (ev "clj4rnx.device = clj4rnx.track:device(" (inc device-index) ")")
  (ev "clj4rnx.parameter = clj4rnx.device:parameter(" (inc (:param-index auto)) ")")
  (ev "clj4rnx.automation = clj4rnx.pattern_track:create_automation(clj4rnx.parameter)")
  (ev "clj4rnx.automation.playmode = renoise.PatternTrackAutomation." playmode)
  (ev "clj4rnx.automation.points = {"
      (->> bars
           (mapcat (fn [index bar] (map #(assoc %1 :t (-> %1 :t (+ index) (* 4 *lpb*) int)) bar)) (iterate inc 0))
           (map #(str "{time=" (->  % :t inc) ", value=" (double (:deg %)) \}))
           (interpose \,)
           (apply str)) \}))

(def pre-patr* {:bar-cnt 1})
(def post-patr* {:bs-- (fn [bars] (map (fn [bar] (map #(update-in % [:oct] (fnil inc 0)) bar)) bars))})

(defn set-patr [{:keys [song tracks offs] :as ctx} patr-index]
  (let [{:keys [name section-name template? bar-cnt] :as patr} (patr-merge pre-patr* (-> song :patrs (get patr-index)) post-patr*)]
    (ev "clj4rnx.pattern = clj4rnx.song:pattern(" (inc patr-index) ")")
    (ev "clj4rnx.pattern.name = '" name "'" )
    (when section-name
      (ev "clj4rnx.song.sequencer:set_sequence_is_start_of_section(" (inc patr-index) ", true)")
      (ev "clj4rnx.song.sequencer:set_sequence_section_name(" (inc patr-index) ", '" section-name "')"))
    (assoc ctx :offs (if template?
                          {}
                          (do
                            (ev "clj4rnx.pattern.number_of_lines = " (* bar-cnt 4 *lpb*))
                            (reduce (fn [offs [track-key {:keys [track-index] :as track}]]
                                      (let [bars (track-key patr)
                                            bars (if (fn? bars) (bars patr track) bars)]
                                        (if-let [bars (seq bars)]
                                          (update-in offs [track-key] (partial set-note-bars track deg-to-pitch)
                                                     (take bar-cnt bars))
                                          (dissoc offs track-key))))
                                    offs tracks))))))

(defn loop-patr- [song patr-index]
  (set-patr song patr-index {})
  (ev "clj4rnx.song.selected_sequence_index = " (inc patr-index))
  (ev "clj4rnx.song.transport.loop_pattern = true")
  (ev "clj4rnx.song.transport:start(renoise.Transport.PLAYMODE_CONTINUE_PATTERN)"))

(defn- set-patrs [song]
  (reduce #(set-patr %1 %2)
          {:song song :tracks (merge (:tracks song) (query-tracks)) :offs {}}
          (range (-> song :patrs count))))

(def grv-1*
     {:bd (parse "1 1 1 1")
      :sd (parse "q 1 q 1")
      :hh-c (parse "e 1e e 1e e 1e e 1e")
      :hc (parse "1 1 1 1e 1e 1 1 1 e s 1s")
      })

(def seeds*
     {:cloudburst (parse 16 "
'(4+w 2w 2-w)
'([3+h 1+h] 1w 1-w) 
'(2+w 7b-w 7b--w)
'(2+h 7b-h 7b--h) '(3+h 1h 1-h)
'([4+h 3+ 4+] 2w 2-w)
'([5+h 1+h] 3w 3-w)
'(6+w 4w 4-w)
'([6+h 5+q. 6+e] 4w 4-w)
'([7b+h 6+ 5+] 5w 5-w)
'([4+h 2+h] 2w 2-w)
'([5+ 6+ 4+ 5+] 3bw 3b-w)
'([4+h 2+h] 2w 2-w)
'([2+h 3+ 4+] 7b-w 7b--w)
'([3+h 1+h] 6-w 6--w)
'(2+ww 2ww 2-ww)")
      :cloudburst-steps (parse "
1s 3s 2s 1s
3s 1s 2s 3s
1s 2s 3s 1s
3s 2s 1s 3s")
      :chords-1 (parse "
'(6-w 1+w 3+w) '(6-w 2+w 4+w) '(5-w 7w 2+w) '(1w 5w 3+w)
'(3w 5w 1+w) '(4w 6w 1+w) '(5-w 7w 2+w) '(1w 5w 3+w)")
      :steps-1 (parse "
1e 2e e 1e 2e e 1e 2e")
      :bs-1 (parse "
'(6-w 1w 5-w) '(2w 4w 1w) '(5-w 7w 7w) '(1w 3w 7b-w)")
      :bs-332 (parse "1e s 1e s 1s s 1e s 1e s 1s s")
      :bs-triadic (parse "1e 1e 3be 1s 3be 1s 3be 5e 5-e
e 1e 3be 1s 3be 1s 3be 5e 5-e")
      :bs-7th-chord (parse "1+s 1s 5s 7be 1s 3bs 5e 1s 5s 7bs 1+s 7bs 5s 1s")
      :bs-pentatonic (parse "1e 1e 5s 7be s 1+e 1s 3be 3bs 4e")
      :bs-pentatonic-fill (parse "1e 1e 5s 7be s 1+e 1s 3be 3bs 4e
1e 1e 5s 7be s 1+e 1s 3be 3bs 4e
1e 1e 5s 7be s 1+e 1s 3be 3bs 4e
1e 1+e 7e 7be 6e 6bs 5e 4s 3e")
      :bs-chromatic-walking (parse "1e 1+e 3be 3b+e 2be 2b+e 2e 2+e
5#-e 5#e 6-e 6e 6#-e 6#e 7-e 7e")
      })

(def patrs*
     [
      (assoc (select-keys grv-1* [:bd :sd :hh-c]) :name "grv-1" :section-name "templates" :bar-cnt 2)
      {:section-name "intro" :bd (partial set-alias 0) }
      {:bd (partial set-alias 0) :sd (partial set-alias 0)}
      #_(assoc (select-keys grv-1* [:bd]) :name "grv-1" :section-name "intro")
      #_(select-keys grv-1* [:bd :sd])
      #_(assoc (select-keys grv-1* [:bd :sd]) :name :grv-2)
      #_{:bs (parse "1e 1+e 7be 1+e 1e 5e 7be 1+e
5e 1+e 7be 1+e 1e 5e 7be 1+e
3be 3b+e 2+e 3b+e 3be 7be 2+e 3b+e
5be 3b+e 2+e 3b+e 5be 7be 3b+e 4+e")
       :bd (parse "1 1 1 1") :hc (parse "q 1 q 1")
       :hh-c (parse "1s 1s s 1s 1s 1s s 1s 1s 1s s 1s 1s 1s s 1s ")
       :hh-o (parse "e 1 1 1 1")
;       :ride (parse "1e 1e 1e 1e 1e 1e 1e 1e")
       :crash (parse 2 "1w") :bar-cnt 8}
      #_grv-1*
      #_(patr-merge grv-1* {:bs (parse "
e 6-e e 6-e e 6-e e 6-e
e 2e e 2e e 2e e 2e
e 7-e e 7-e e 7-e e 7-e
e 1e e 1e e 1e e 1e
") :bar-cnt 8})
      #_(patr-merge grv-1* {:bs (step-seq step-index (parse "1e 1e 1s 1s s -1s 1e 2e 1s 1s") (:bs-1 seeds*)) :bar-cnt 8})
      #_{:bs (step-seq step-index (parse "1e 1e 1s 1s s -1s 1e 2e 1s 1s") (:bs-1 seeds*))
;       :ld (step-seq (partial step-index >) (:steps-1 seeds*) (:chords-1 seeds*))
;       :pd (step-seq (partial step-index >) (parse "1s 1s '(1e 2e)") (:chords-1 seeds*))
;       :pd (step-seq (partial step-index nil) (parse "1s -1s '(1e 2e)") (parse "'(1w 3w 5w 6w 7b-w)"))
;       :pd (:chords-1 seeds*)
       :bar-cnt 8}
      #_{:bs (step-seq (partial step-index <) (parse "1e 1e 1s 1s s 1s 1e 1e 1s 1s") (:cloudburst seeds*)) :pd (:cloudburst seeds*) :bar-cnt 16}
      #_(-> grv-1* (select-keys [:bd]) (assoc :bar-cnt- 1))
      #_(assoc (patr-merge grv-1* bs-1*)
        :pd (apply step-seq step-all ((juxt :steps-1 :cloudburst) seeds*))
        :bar-cnt 16)
      #_(assoc (patr-merge grv-1* bs-1*)
        :key (apply step-seq step-index ((juxt :cloudburst-steps :cloudburst) seeds*))
        :bar-cnt 16)
      #_(assoc (patr-merge grv-1* bs-1* {:pd (:cloudburst seeds*)}) :bar-cnt 16)
      ])

(defn demo []
  (def song*
       {:bpm 140 
        :tracks {:bs {:note-f #(update-in % [:oct] (fnil (partial + 1) 0))}} 
        :patrs patrs*
        })
  (reset-song song*)
  (set-patrs song*)
  nil)

(bootstrap)

(defonce *interactive* false)
;(if *interactive* (loop-patr song* 0) (demo))
(demo)
