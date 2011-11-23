(ns clj4rnx
  "
"
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.contrib.math :as math]
   [clojure.contrib.logging :as log]
   [osc :as osc]))

(def *lpb* 4)

(osc/osc-debug false)
(defonce *client* (osc/osc-client "127.0.0.1" 8000))

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

(defn reset-instr [{:keys [id instr-idx] :as instr}]
  (ev "clj4rnx.instrument = (renoise.song().instruments[" (inc instr-idx) "] or renoise.song():insert_instrument_at(" (inc instr-idx) "))")
  (when (:sample-filename instr)
    (ev "clj4rnx.instrument:sample(1).sample_buffer:load_from('" (:sample-filename instr) "')")
    (ev (str "clj4rnx.instrument.name = '" (str (name id) ": " (-> :sample-filename instr io/file .getName)) "'")))
  (when (:plugin-name instr)
    (ev "clj4rnx.plugin_properties = clj4rnx.instrument.plugin_properties")
      (ev "clj4rnx.plugin_properties:load_plugin('" (:plugin-name instr) "')")
      (ev "clj4rnx.plugin_properties.plugin_device.active_preset = " (:preset instr))
      (ev (str "clj4rnx.instrument.name =  '" (str (name id)) ": ' .. clj4rnx.instrument.name"))))

(defn reset-song [song]
  (ev "clj4rnx.song = renoise.song()")
  (ev "clj4rnx.song.transport.bpm = " (:bpm song))
  (ev "clj4rnx.song.transport.lpb = " *lpb*)
  (ev "clj4rnx.sequencer = clj4rnx.song.sequencer")

  (ev "
do local trk_index = #renoise.song ().tracks
  while renoise.song():track(trk_index).type ~= renoise.Track.TRACK_TYPE_SEQUENCER do trk_index = trk_index - 1 end
  while trk_index < " (-> song :tracks count) " do renoise.song():insert_track_at(trk_index) trk_index = trk_index + 1 end
end")

  (ev "
do local patr_index = #renoise.song().patterns + 1
  while patr_index <= " (-> song :patrs count) " do
    renoise.song().sequencer:insert_sequence_at(patr_index, patr_index)
    patr_index = patr_index + 1
  end
end")

  (dotimes [n (-> song :tracks count)]
    (let [{:keys [id instr devices]} (-> song :tracks (get n))]
      (ev "clj4rnx.track = clj4rnx.song:track(" (inc n) ")")
      (ev "clj4rnx.track.name = '" (name id) "'")
      (reset-instr (assoc instr :id id :instr-idx n))
      #_(dorun (map-indexed (fn [device-idx device-name]
                            (ev "clj4rnx.track:insert_device_at('" device-name "', " (+ device-idx 2) ")"))
                          devices)))))

(defn- note-col [t off-t off-vec]
  (let [index (or  (->> off-vec
                        (map-indexed vector)
                        (filter (fn [[index off-t]] (when (>= t off-t) index)))
                        ffirst)
                   (count off-vec))]
    [index (assoc off-vec index off-t)]))

(defn set-notes [trk-idx pitch-f off-vec notes]
  (ev "clj4rnx.track = clj4rnx.song:track(" trk-idx ")")
  (ev "clj4rnx.pattern_track = clj4rnx.pattern:track(" trk-idx ")")
  (ev "
for i,v in ipairs(clj4rnx.pattern_track.lines) do
  v:clear()
end")
  (dorun (map-indexed
          (fn [index off-t]
            (ev "clj4rnx.note_column = clj4rnx.pattern_track:line(" (inc (int (* off-t 4 *lpb*))) "):note_column(" (inc index) ")")
            (ev "clj4rnx.note_column.note_string = 'OFF'" ))
          off-vec))
  (let [off-vec (reduce (fn [off-vec {:keys [t deg oct acc v d a] :as e :or {oct 0 v 3/4 acc 0 a 1}}]
                          (let [l (inc (int (* t 4 *lpb*)))
                                [note-col off-vec] (note-col t (+ t (* d a)) off-vec)
                                note-col (inc note-col)]
                            (ev "clj4rnx.note_column = clj4rnx.pattern_track:line(" l "):note_column(" note-col ")")
                            (ev "clj4rnx.note_column.note_value = " (pitch-f deg oct acc))
                            (ev "clj4rnx.note_column.volume_value = " (math/round (* v 254)))
                            (ev "clj4rnx.note_column.instrument_value = " (dec trk-idx))
                            (ev "clj4rnx.note_column = clj4rnx.pattern_track:line(" (int (+  l (* d 4 *lpb* a))) "):note_column(" note-col ")")
                            (ev "clj4rnx.note_column.note_string = 'OFF'" )
                            off-vec))
                          (or off-vec []) notes)
        vis-note-cols (count off-vec)]
      (ev "if clj4rnx.track.visible_note_columns < " vis-note-cols
          " then clj4rnx.track.visible_note_columns = " vis-note-cols " end")
      off-vec))

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
                          (let [idx (int t)
                                bars (into bars (repeat (- (inc idx) (count bars)) []))]
                            (assoc bars idx (conj (bars idx) (assoc e :t (- (e :t) idx)))))) [] es)]
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

(defn set-note-bars [trk-idx pitch-f note-f off-vec bars]
  (let [off-vec (set-notes trk-idx pitch-f off-vec
                           (map (or note-f identity)
                                (mapcat (fn [idx bar] (map #(update-in % [:t] (partial + idx)) bar))
                                        (iterate inc 0) bars)))
        bars-t (count bars)]
    (->> off-vec (map #(- % bars-t)) (remove neg?) vec)))

(defn set-auto-bars [{:keys [device-index param-index playmode] :as auto :or {playmode "PLAYMODE_LINEAR"}} bars]
  (ev "clj4rnx.device = clj4rnx.track:device(" (inc device-index) ")")
  (ev "clj4rnx.parameter = clj4rnx.device:parameter(" (inc (:param-index auto)) ")")
  (ev "clj4rnx.automation = clj4rnx.pattern_track:create_automation(clj4rnx.parameter)")
  (ev "clj4rnx.automation.playmode = renoise.PatternTrackAutomation." playmode)
  (ev "clj4rnx.automation.points = {"
      (->> bars
           (mapcat (fn [idx bar] (map #(assoc %1 :t (-> %1 :t (+ idx) (* 4 *lpb*) int)) bar)) (iterate inc 0))
           (map #(str "{time=" (->  % :t inc) ", value=" (double (:deg %)) \}))
           (interpose \,)
           (apply str)) \}))

(def pre-patr*
     {:bar-cnt 1})

(def post-patr* {:bs-- (fn [bars] (map (fn [bar] (map #(update-in % [:oct] (fnil inc 0)) bar)) bars))
                 :bs- (fn [bars] (map (fn [bar] (map (fn [n]
                                                      (assoc n :oct 4)
                                                      #_(if-let [oct (:oct n)]
                                                        (cond
                                                         (<= oct 3) (assoc n :oct 4)
                                                         (>= oct 5) (assoc n :oct 4)
                                                         :else n)
                                                        n))  bar)) bars))})
(defn set-patr [song patr-idx off-map]
  (let [{:keys [ bar-cnt] :as patr} (patr-merge pre-patr* (-> song :patrs (get patr-idx)) post-patr*)]
    (ev "clj4rnx.pattern = clj4rnx.song:pattern(" (inc patr-idx) ")")
    (ev "clj4rnx.pattern.number_of_lines = " (* bar-cnt 4 *lpb*))
    (->> (:tracks song) (map-indexed vector)
         (reduce (fn [off-map [index {:keys [id automation note-f]}]]
                   (doseq [auto automation] (set-auto-bars auto (take bar-cnt (-> auto :id patr))))
                   (update-in off-map [id] (partial set-note-bars (inc index) deg-to-pitch note-f)
                              (take bar-cnt (when-let [bars (id patr)] (when (seq? bars) bars)))))
                 off-map))))

(defn loop-patr [song patr-idx]
  (set-patr song patr-idx {})
  (ev "clj4rnx.song.selected_sequence_index = " (inc patr-idx))
  (ev "clj4rnx.song.transport.loop_pattern = true")
  (ev "clj4rnx.song.transport:start(renoise.Transport.PLAYMODE_CONTINUE_PATTERN)"))

(defn- set-patrs [song]
  (reduce (fn [off-map index] (set-patr song index off-map)) {} (range (-> song :patrs count))))

(def grv-1*
     {:bd (parse "1 1 1 1")
;      :bd-vol (parse "1/4 1/2 3/4 1")
      :sd (parse "q 1 q 1")
      :hh-c (parse "e 1e e 1e e 1e e 1e")
;      :hc (parse "1 1 1 1e 1e 1 1 1 1e s 1s")
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
      {:bs (parse "1e 1+e 7be 1+e 1e 5e 7be 1+e
5e 1+e 7be 1+e 1e 5e 7be 1+e
3be 3b+e 2+e 3b+e 3be 7be 2+e 3b+e
5be 3b+e 2+e 3b+e 5be 7be 3b+e 4+e")
       :bd (parse "1 1 1 1") :hc (parse "q 1 q 1")
       :hh-c (parse "1s 1s s 1s 1s 1s s 1s 1s 1s s 1s 1s 1s s 1s ")
       :hh-o (parse "e 1 1 1 1")
;       :ride (parse "1e 1e 1e 1e 1e 1e 1e 1e")
       :crash (parse 2 "1w") :bar-cnt 8}
      (select-keys grv-1* [:bd])
      (select-keys grv-1* [:bd :sd])
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
        :tracks [
                 #_{:id :bs :note-f #(update-in % [:oct] (fnil (partial + 1) 0))
                  :instr {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 87}}
                 {:id :bd
                  :instr {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Bassdrums/VEC1 Trancy/VEC1 BD Trancy 10.wav"}
                  :devices- ["Audio/Effects/    Native/Delay"]
                  :automation- [{:id :bd-vol :device-index 0 :param-index 1}
;                                {:id :bd-pan :device-index 0 :param-index 0}
                               ]}
                 {:id :sd
                  :instr {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Snares/VEC1 Snare 031.wav"}}
                 #_{:id :hc
                  :instr {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Claps/VEC1 Clap 027.wav"}}
                 #_{:id :hh-c
                  :instr {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Cymbals/VEC1 Close HH/VEC1 Cymbals  CH 11.wav"}}
                 #_{:id :hh-o
                  :instr {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Cymbals/VEC1 Open HH/VEC1 Cymbals  OH 001.wav"}}
                 #_{:id :ride
                  :instr {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Cymbals/VEC1 Ride/VEC1 Cymbals RD 05.wav"}}
                 #_{:id :crash
                  :instr {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Cymbals/VEC1 Crash/VEC1 Cymbals CR 001.wav"}}
                 #_{:id :pd
                  :instr {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 32}}                 
                 #_{:id :key
                  :instr {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 27}}
                 #_{:id :ld
                  :instr {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 46}}                 
                 #_{:id :hov
                  :instr {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 83}} 
                 ]
        :patrs patrs*
        })
  (reset-song song*)
  (set-patrs song*))

(defonce *interactive* false)
(if *interactive* (loop-patr song* 0) (demo))
