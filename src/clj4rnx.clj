(ns clj4rnx
  "
  TODO:
  articulation 1ea0.75v1 1e:a0.75:v1 1e 1e.75{
  accidentals
  velocity/volumne
  shape velocity
  feel
"
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.contrib.math :as math]
   [clojure.contrib.logging :as log]
   [osc :as osc]))

(osc/osc-debug true)

(def *lpb* 16)
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

(defn ev [& args]
  (osc/osc-send *client* "/renoise/evaluate" (apply str args)))

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
;    (ev "clj4rnx.pattern.number_of_lines = " (* 4 *lpb*))
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

; (demo)

(defn- note-col [t off-t off-vec]
;  (println "note-col:" t off-t off-vec)
  (let [index (or  (->> off-vec
                        (map-indexed vector)
                        (filter (fn [[index off-t]] (when (>= t off-t) index)))
                        ffirst)
                   (count off-vec))]
    [index (assoc off-vec index off-t)]))

(defn set-notes [trk-idx pitch-f notes]
  (ev "clj4rnx.track = clj4rnx.song:track(" trk-idx ")")
  (ev "clj4rnx.pattern_track = clj4rnx.pattern:track(" trk-idx ")")
  (ev "clj4rnx.pattern_track:clear()")
  (let [off-vec (reduce (fn [off-vec {:keys [t deg oct v d] :as e :or {oct 0 v 3/4}}] 
                          (let [l (inc (int (* t 4 *lpb*)))
                                [note-col off-vec] (note-col t (+ t d) off-vec)
                                note-col (inc note-col)]
                            (ev "clj4rnx.note_column = clj4rnx.pattern_track:line(" l "):note_column(" note-col ")")
                            (ev "clj4rnx.note_column.note_value = " (pitch-f deg oct))
                            (ev "clj4rnx.note_column.volume_value = " (math/round (* v 254)))
                            (ev "clj4rnx.note_column.instrument_value = " (dec trk-idx))
                            (ev "clj4rnx.note_column = clj4rnx.pattern_track:line(" (int (+  l (* d 4 *lpb*))) "):note_column(" note-col ")")
                            (ev "clj4rnx.note_column.note_string = 'OFF'" )
                            off-vec))
                        [] notes)
        vis-note-cols (count off-vec)]
    (ev "if clj4rnx.track.visible_note_columns < " vis-note-cols
        " then clj4rnx.track.visible_note_columns = " vis-note-cols " end")))

;(def e-re* #"([\d/\.]+)?([#b]+)?([+-]+)?([whqest\.]+)?([\d/\.]+)?\s*(\{.*})?")
(def e-re* #"([\d/\.]+)?([#b]+)?([+-]+)?([whqest\.]+)?([\d/\.]+)?")
; (re-find e-re* "1bbet {:b :abacab}")
; (map #(re-find e-re* %) ["1bbet {:b :abacab}" "1#0.5{:a 2}" "1b""1/4" "0.5" "1" "2+" "7--" "e"])

(defn- parse-e [s]
  (when-not (str/blank? (log/spy s))
    (if-let [[_ deg acc oct d v m] (re-find e-re* s)]
      (let [acc (reduce #(+ %1 ({\# 1 \b -1} %2)) 0 acc)
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

(comment (defn- parse-e [s]
           (when-not (str/blank? (log/spy s))
             (if-let [[_ deg acc oct d v m] (re-find e-re* s)]
               (let [acc (reduce #(+ %1 ({\# 1 \b -1} %2)) 0 acc)
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
               (throw (IllegalArgumentException. (str "parse-e: unable to parse " s)))))))

; (parse-e "1bbe0.5 {:b :abacab}")
; (parse-e "1q")

(defn- to-e [x]
  (cond
   (string? x) (parse-e x)
   (vector? x) (->> x (map to-e) vec)
   (set? x) (->> x (map to-e) set)
   :else (throw (IllegalArgumentException. (str "to-e: unexpected arg: " x)))))

(defn- to-es
  ([x] (to-es {:t 0 :es []} x))
  ([ctx x]
     (cond
      (map? x) (conj ctx [:t (+ (:t ctx) (:d x))] (when (:deg x) [:es (conj (:es ctx) (assoc x :t (:t ctx)))]))
      (vector? x) (reduce (fn [ctx x] (to-es ctx x)) ctx x)
      (set? x) (let [new-ctx (reduce (fn [{max-t :max-t :as new-ctx} val]
                                       (let [new-ctx (to-es (assoc new-ctx :t (:t ctx)) val)]
                                         (assoc new-ctx :max-t (max (:t new-ctx) (:max-t new-ctx) max-t))))
                                     (assoc ctx :es [] :max-t (:t ctx)) x)]
                 (assoc new-ctx :t (:max-t new-ctx) :es (concat (:es ctx) (sort #(compare (:t %1) (:t %2)) (:es new-ctx))))))))

(defn- es-to-bars [es]
  (reduce (fn [bars {t :t :as e}]
            (let [idx (int t)
                  bars (into bars (repeat (- (inc idx) (count bars)) []))]
              (assoc bars idx (conj (bars idx) (assoc e :t (- (e :t) idx)))))) [] es))

(defn- normalize [s] (str \[ (str/replace s e-re* #(if-not (-> 0 % empty?) (str \" (% 0) \") "")) \]))

(defn- parse [s] (->> s normalize read-string to-e to-es :es es-to-bars repeat (mapcat identity)))

; (->> "#{1w 5w [1+h 2+h]}" normalize read-string to-e to-es :es)
; (->> "[1 1 1 1]" normalize read-string to-e to-es :es)
; (->> "#{1w [5 6 7 8]}" normalize read-string to-e to-es :es)
; (take 1  (parse "#{[1+h 2+h]}"))
; (take 2 (parse "#{1w [1+h 2+h]}"))
; (take 2 (parse "1 1 1 1"))

(def bar-fs* (ref {}))
(defn add-bar-f [name f] (dosync (alter bar-fs* assoc name f)) nil)
(defn get-bars [name] ((@bar-fs* name)))

(def patr-fs* (ref {}))
(defn add-patr-f [name f] (dosync (alter patr-fs* assoc name f)))
(defn get-patr-f [name] (@patr-fs* name))

(add-patr-f :grv-1-full (fn []
                          {:bd (parse "1 1 1 1")
                           :bd-vol (parse "1/4 1/2 3/4 1")
                           :sd (parse "q 1 q 1")
                           :hh-c (parse "e 1e e 1e e 1e e 1e")
                           :hc (parse "1 1 1 1e 1e 1 1 1 1e s 1s")
                           :bass (parse "e 1+e e 1+e e 1+e e 1+e
e 5e e 5e e 5e e 5e
e 6e e 6e e 6e e 6e
e 4e e 4e e 4e e 4e")
                           :hov (parse "#{1q 5q}")
                           :pad (parse "#{1w [1+h 2+h]}
#{1w [5h 6h]}
#{1w [6h 5h]}
#{1w [4h 5h]}")}))

; (demo)
; (loop-patr 0)

(def patrs*
     [{:patr-f (fn [] (select-keys ((get-patr-f :grv-1-full)) [:bd :bd-vol])) :bar-cnt 2}
;      {:patr-f (fn [] (select-keys ((get-patr-f :grv-1-full)) [:bd :hh-c]))  :bar-cnt 1}
;      {:patr-f (fn [] (select-keys ((get-patr-f :grv-1-full)) [:bd :hh-c :sd])) :bar-cnt 1}
;      {:patr-f (get-patr-f :grv-1-full) :bar-cnt 4}
      ])

(defn- deg-to-pitch
  ([deg oct]
     (deg-to-pitch 48 deg oct))
  ([base deg oct]
      (+ base ([0 2 4 5 7 9 11] (dec deg)) (* oct 12))))

(defn set-note-bars [trk-idx pitch-f bars]
  (set-notes trk-idx pitch-f
             (mapcat
              (fn [idx bar]
                (map #(assoc %1 :t (+ idx (:t %1))) bar))
              (iterate inc 0)  bars)))

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
        (set-note-bars (inc index) deg-to-pitch (take bar-cnt (-> track :id m)))
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
                 {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 29}]
        :tracks [{:id :bd :devices- ["Audio/Effects/    Native/Delay"]
                  :automation- [{:id :bd-vol :device-index 0 :param-index 1}
;                                {:id :bd-pan :device-index 0 :param-index 0}
                               ]}
                 {:id :sd}
                 {:id :hh-c}
                 {:id :hc}
                 {:id :bass}
                 {:id :hov}
                 {:id :pad}
                 ]
        :patrs patrs*
        })
  (reset-song song*)
  (set-patrs song*)
  )

; (demo)

(defonce *interactive* false)
(when *interactive* (loop-patr 0))


