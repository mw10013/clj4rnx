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

(osc/osc-debug true)
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

(defn reset-instr [song]
  (ev "clj4rnx.instrument = renoise.song():instrument(" (inc (:instr-idx song)) ")")
  (ev "clj4rnx.instrument:clear()")
  (when (:sample-filename song)
    (ev "clj4rnx.instrument:sample(1).sample_buffer:load_from('" (:sample-filename song) "')")
    (ev (str "clj4rnx.instrument.name = '" (-> :sample-filename song io/file .getName) "'")))
  (if-not (:plugin-name song)
    (ev "clj4rnx.instrument.plugin_properties:load_plugin('')")
    (do
      (ev "clj4rnx.plugin_properties = clj4rnx.instrument.plugin_properties")
      (ev "clj4rnx.plugin_properties:load_plugin('" (:plugin-name song) "')")
      (ev "clj4rnx.plugin_properties.plugin_device.active_preset = " (:preset song)))))

(defn reset-song [song]
  (ev "clj4rnx.song = renoise.song()")
  (ev "clj4rnx.song.transport.bpm = " (:bpm song))
  (ev "clj4rnx.song.transport.lpb = " *lpb*)
  (ev "clj4rnx.sequencer = clj4rnx.song.sequencer")
  (doseq [_ (range 25)]
    (ev "clj4rnx.sequencer:delete_sequence_at(" 1 ")")
    (ev "clj4rnx.song:delete_track_at(" 1 ")")
    (ev "clj4rnx.song:delete_instrument_at(" 1 ")"))
  
  (doseq [idx (range 1 (-> song :patrs count inc))]
    (when-not (= idx 1)
      (ev "clj4rnx.sequencer:insert_sequence_at(" idx ", " idx ")"))
    (ev "clj4rnx.pattern = clj4rnx.song:pattern(" idx ")")
    (ev "clj4rnx.pattern:clear()"))

  (doseq [_ (range (-> song :tracks count))]
    (ev "clj4rnx.track = clj4rnx.song:insert_track_at(" 1 ")"))

  (dotimes [n (-> song :tracks count)]
          (let [{:keys [id index devices]} (-> song :tracks (get n))]
            (ev "clj4rnx.track = clj4rnx.song:track(" (inc n) ")")
            (ev "clj4rnx.track.name = '" (name id) "'")
            (dorun (map-indexed (fn [device-idx device-name]
                                  (ev "clj4rnx.track:insert_device_at('" device-name "', " (+ device-idx 2) ")"))
                                devices))))

  (dotimes [_ (-> song :instrs count dec)]
    (ev "clj4rnx.song:insert_instrument_at(" 1 ")"))
  (dorun (map-indexed (fn [idx m] (reset-instr (assoc m :instr-idx idx))) (:instrs song))))

(comment (defn- note-col [t off-t off-vec]
           (let [index (or  (->> off-vec
                                 (map-indexed vector)
                                 (filter (fn [[index off-t]] (when (>= t off-t) index)))
                                 ffirst)
                            (count off-vec))]
             [index (assoc off-vec index off-t)])))

(defn- note-col [t off-t off-vec]
  (let [index (or  (->> off-vec
                        (map-indexed vector)
                        (filter (fn [[index off-t]] (when (>= t off-t) index)))
                        ffirst)
                   (count off-vec))]
    [index (assoc off-vec index off-t)]))

; (note-col 1/4 1/2 [1/4 1/4])
; (demo)

(defn set-notes [trk-idx pitch-f notes]
  (ev "clj4rnx.track = clj4rnx.song:track(" trk-idx ")")
  (ev "clj4rnx.pattern_track = clj4rnx.pattern:track(" trk-idx ")")
  (ev "clj4rnx.pattern_track:clear()")
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
                        [] notes)
        vis-note-cols (count off-vec)]
    (ev "if clj4rnx.track.visible_note_columns < " vis-note-cols
        " then clj4rnx.track.visible_note_columns = " vis-note-cols " end")))

(def e-re* #"(([whqest\.]+)|([\d/\.]+)([#b]+)?([+-]+)?([whqest\.]+)?([\d/\.]+)?\s*(\{.*})?)")
; (normalize "('(1 3))")
; (re-find e-re* "list")
; (re-find e-re* "ww")
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

; (to-es ['({:deg 1, :d 1/4} {:deg 3, :d 1/4}) {:deg 4, :d 1/4}])
; (->> "1 3" normalize read-string eval to-e)
; (->> "'(1 3)" normalize read-string eval to-e)
; (->> "'(3) 4" normalize read-string eval to-e)
; (->> "'(1 3) 4" normalize read-string eval to-e)
; (->> "'(1 3) 4" normalize read-string eval to-e to-es)

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

; (take 2 (parse "'(1 3 5) 6"))
; (take 2 (parse "'(1w 3w 5w)"))
; (->> "1wwww" normalize read-string eval to-e to-es :es (es-to-bars 2))
; (take 2  (parse "'([1+h 2+h])"))
; (take 1  (parse "'([1+h 2+h] 1w)"))

(defn- step-all [step buf]
  (let [{:keys [t d]} (first step)]
    (map #(assoc % :t t :d d) buf)))

; (step-all [{:t 1/4 :d 1/4}] '({:t 0, :deg 1, :d 1/2} {:t 0, :deg 5, :d 1/2}))

(defn- step-index [step buf]
  (reduce (fn [coll index]
            (if-let [; n (nth buf (- (count buf) (:deg index)) nil)
                     n (nth buf (dec (:deg index)) nil)]
              (conj coll (conj n [:t (:t index)] [:d (:d index)] (if (:oct index) [:oct (:oct index)])))
              coll)) [] step))

(comment (take 2 (step-seq step-index
                           (parse "1s 2-s 2s 1s
2-s 1s 2s 2-s
1s 2s 2-s 1s
2-s 2s 1s 2-s")
                           (parse "'(2+w 4w)")))
         )
; (take 2 (step-seq step-index(parse "1e 2e 2-e 1") (parse "'(1h 5h)")))
; (step-index [{:t 1/2 :deg 2 :d 1/4}] [{:t 0 :deg 2 :d 1} {:t 0 :deg 22 :d 1}])
; (take 1 (step-seq step-index (parse "1e 2e 1e") (parse "'(1h 3h 5h)")))

(defn- step-seq
  ([] (take 2 (step-seq (parse "1 1s 1s") (parse "'(1e 1+e) 2e 3e"))))
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

; (take 2 (step-seq (parse "1") (parse "'(1h 5h)")))
; (step-seq)
; (map flatten (step-seq))
; (->> (parse "1 1s 1s") first (group-by :t))
; (->> (parse "1 1s 1s") first (group-by :t) (map second))
; (take 2 (step-seq (parse "1 1 s 1s") (parse "'(1h 5h) '(1h 4h) '(1h 3h) '(1h 4h)")))
; (take 2 (step-seq (parse "1 1 s 1s") (parse "'(1h 5h) '(1h 4h) '(1h 3h) '(1h 4h)")))

(defn- arp-index-seq [indexes octs]
  "Returns seq of index oct pairs"
  (cycle (apply concat ((juxt identity
                              (constantly [[(first indexes) (inc (last octs))]])
                              #(butlast (rseq (vec %)))) (for [oct octs index indexes] [index oct])))))

; (take 25 (arp-index-seq [1 3 5] (range 3)))

(defn- arp-seq
  ([] (arp-seq {:octs (range 3)} (take 2 (step-seq (parse "1e 1e 1e 1e 1e") (parse "'(1w 3w 5w)")))))
  ([ctx step-bars]
     (lazy-seq
      (when (seq step-bars)
        (let [{b :b :as ctx}
              (reduce (fn [{:keys [prev-cnt octs b] :as ctx} step]
                        (let [cnt (count step)
                              ctx (update-in ctx [:indexes] #(cond
                                                              (zero? cnt) nil
                                                              (= cnt prev-cnt) %
                                                              :else (arp-index-seq (range cnt) octs)))
                              [index oct] (first (:indexes ctx))
                              ctx (update-in ctx [:indexes] next)]
                          (conj ctx [:prev-cnt cnt] (when index [:b (conj (:b ctx) (assoc (nth step index) :oct oct))]))))
                      (assoc ctx :b [])
                      (first step-bars))]
          (cons b (arp-seq ctx (rest step-bars))))))))

; (take 2 (arp-seq))

(def bar-fs* (ref {}))
(defn add-bar-f [name f] (dosync (alter bar-fs* assoc name f)) nil)
(defn get-bars [name] ((@bar-fs* name)))

(def patr-fs* (ref {}))
(defn add-patr-f [name f] (dosync (alter patr-fs* assoc name f)))
(defn get-patr-f [name] (@patr-fs* name))

(def bars*
     {:cb-ss-1 (parse "
1s 3s 2s 1s
3s 1s 2s 3s
1s 2s 3s 1s
3s 2s 1s 3s
")
      :cb-1 (parse "
'(4+w 2w 2-w)
'(3+h 1h 1-h) '(1+h 1h 1-h)
'(2+w 7b-w 7b--w)
'(2+h 7b-h 7b--h) '(3+h 1h 1-h)
")
      :cb-2 (parse "
'(4+h 2h 2-h) '(3+ 2 2-) '(4+ 2 2-)
'(5+h 3h 3-h) '(1+h 3h 3-h)
'(6+w 4w 4-w)
'(6+h 4h 4-h) '(5+q. 4q. 4-q.) '(6+e 4e 4-e)
")
      })

(add-patr-f :grv-1-full (fn []
                          {:bd (parse "1 1 1 1")
                           :bd-vol (parse "1/4 1/2 3/4 1")
                           :sd (parse "q 1 q 1")
                           :hh-c (parse "e 1e e 1e e 1e e 1e")
                           :hc (parse "1 1 1 1e 1e 1 1 1 1e s 1s")
                           :bass- (parse "1+h0.45 {:a 1.25}  5+h1")
                           :bass (parse "e 1+e e 1+e e 1+e e 1+e
e 5e e 5e e 5e e 5e
e 6e e 6e e 6e e 6e
e 4e e 4e e 4e e 4e")
                           :hov (parse "'(1q 5q)")
                           :pad (parse "'(1w [1+h 2+h])
'(1w [5h 6h])
'(1w [6h 5h])
'(1w [4h 5h])")
                           }))

(add-patr-f :cb-1 (fn[] {:bell (step-seq step-index (:cb-ss-1 bars*) (:cb-1 bars*))}))
(add-patr-f :cb-2 (fn[] {:bell (step-seq step-index (:cb-ss-1 bars*) (:cb-2 bars*))}))

; (demo)
; (take 2 (arp-seq (range 3) (step-seq (parse "1 1 s 1s") (parse "'(1h 5h) '(1h 4h) '(1h 3h) '(1h 4h)"))))

(def patrs*
     [{
;       :patr-f (fn [] (merge (select-keys ((get-patr-f :grv-1-full)) [:bd]) (select-keys ((get-patr-f :cb-1)) [:bell]))) :bar-cnt 4
       :patr-f (fn [] (merge (select-keys ((get-patr-f :grv-1-full)) [:bd]) (select-keys ((get-patr-f :cb-2)) [:bell]))) :bar-cnt 4}
;      {:patr-f (fn [] (select-keys ((get-patr-f :grv-1-full)) [:bd :hh-c]))  :bar-cnt 1}
;      {:patr-f (fn [] (select-keys ((get-patr-f :grv-1-full)) [:bd :hh-c :sd])) :bar-cnt 1}
;      {:patr-f (get-patr-f :grv-1-full) :bar-cnt 4}
      ])

(defn- deg-to-pitch
  ([deg oct acc]
     (deg-to-pitch 48 deg oct acc))
  ([base deg oct acc]
      (+ base ([0 2 4 5 7 9 11] (dec deg)) (* oct 12) acc)))

(defn set-note-bars [trk-idx pitch-f note-f bars]
  (set-notes trk-idx pitch-f
             (map (or note-f identity)
                  (mapcat (fn [idx bar] (map #(update-in % [:t] (partial + idx)) bar))
                          (iterate inc 0) bars))))

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

; (demo)

(defn set-patr [song idx]
  (let [{:keys [patr-f bar-cnt]} (-> song :patrs (get idx))
        m (patr-f)]
    (ev "clj4rnx.pattern = clj4rnx.song:pattern(" (inc idx) ")")
    (ev "clj4rnx.pattern.number_of_lines = " (* bar-cnt 4 *lpb*))
    (dorun
     (map-indexed
      (fn [index track]
        (set-note-bars (inc index) deg-to-pitch (-> track :note-f) (take bar-cnt (-> track :id m)))
        (doseq [auto (:automation track)] (set-auto-bars auto (take bar-cnt (-> auto :id m)))))
      (:tracks song)))))

(defn loop-patr [song idx]
  (set-patr song idx)
  (ev "clj4rnx.song.selected_sequence_index = " (inc idx))
  (ev "clj4rnx.song.transport.loop_pattern = true")
  (ev "clj4rnx.song.transport:start(renoise.Transport.PLAYMODE_CONTINUE_PATTERN)"))

(defn- set-patrs [song]
  (dotimes [n (-> song :patrs count)] (set-patr song n)))

(defn demo []
  (def song*
       {:bpm 140 
        :instrs [{:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Bassdrums/VEC1 Trancy/VEC1 BD Trancy 10.wav"}
                 {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Snares/VEC1 Snare 031.wav"}
                 {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Cymbals/VEC1 Close HH/VEC1 Cymbals  CH 11.wav"}
                 {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Cymbals/VEC1 Open HH/VEC1 Cymbals  OH 001.wav"}
                 {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Claps/VEC1 Clap 027.wav"}
                 {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 87}
                 {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 83}
                 {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 38}]
        :tracks [{:id :bd
                  :devices- ["Audio/Effects/    Native/Delay"]
                  :automation [{:id :bd-vol :device-index 0 :param-index 1}
;                                {:id :bd-pan :device-index 0 :param-index 0}
                               ]}
                 {:id :sd}
                 {:id :hh-c}
                 {:id :hh-o}
                 {:id :hc}
                 {:id :bass :note-f #(update-in % [:oct] (fnil (partial + 0) 0))}
                 {:id :hov} 
;                 {:id :pad}
                 {:id :bell}
                 ]
        :patrs patrs*
        })
  (reset-song song*)
  (set-patrs song*))

; (demo)

(defonce *interactive* false)
(when *interactive* (loop-patr song* 0))

