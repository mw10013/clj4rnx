(ns clj4rnx
  "
  TODO:
  shape velocity
  accidentals
  velocity/volumne
  articulation
  feel
  triplets
  dotted
  automation
"
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.contrib.math :as math]
   [osc :as osc]))

(osc/osc-debug true)

(def *lpb* 4)
(defonce *client* (osc/osc-client "127.0.0.1" 8000))

(defn ev [& args]
  (osc/osc-send *client* "/renoise/evaluate" (apply str args)))

; rprint(renoise.song():instrument(1).plugin_properties.available_plugins)
; (ev "print(renoise.song():instrument(1).plugin_properties.plugin_name)")
; (ev "print(renoise.song():instrument(1).plugin_properties.plugin_device.active_preset)")
; rprint(renoise.song():instrument(1).plugin_properties.plugin_device.presets)
; Audio/Generators/VST/Nexus
; Audio/Generators/VST/Sylenth1

(defn reset-instr [ctx]
  (ev "clj4rnx.instrument = renoise.song():instrument(" (inc (:instr-idx ctx)) ")")
  (ev "clj4rnx.instrument:clear()")
  (when (:sample-filename ctx)
    (ev "clj4rnx.instrument:sample(1).sample_buffer:load_from('" (:sample-filename ctx) "')")
    (ev (str "clj4rnx.instrument.name = '" (-> :sample-filename ctx io/file .getName) "'")))
  (if-not (:plugin-name ctx)
    (ev "clj4rnx.instrument.plugin_properties:load_plugin('')")
    (do
      (ev "clj4rnx.plugin_properties = clj4rnx.instrument.plugin_properties")
      (ev "clj4rnx.plugin_properties:load_plugin('" (:plugin-name ctx) "')")
      (ev "clj4rnx.plugin_properties.plugin_device.active_preset = " (:preset ctx)))))

(defn reset-song [ctx]
  (ev "clj4rnx.song = renoise.song()")
  (ev "clj4rnx.song.transport.bpm = " (:bpm ctx))
  (ev "clj4rnx.sequencer = clj4rnx.song.sequencer")
  (doseq [_ (range 25)]
    (ev "clj4rnx.sequencer:delete_sequence_at(" 1 ")")
    (ev "clj4rnx.song:delete_track_at(" 1 ")")
    (ev "clj4rnx.song:delete_instrument_at(" 1 ")"))
  
  (doseq [idx (range 1 (inc (:patr-cnt ctx)))]
    (when-not (= idx 1)
      (ev "clj4rnx.sequencer:insert_sequence_at(" idx ", " idx ")"))
    (ev "clj4rnx.pattern = clj4rnx.song:pattern(" idx ")")
    (ev "clj4rnx.pattern.number_of_lines = " (* 4 *lpb*))
    (ev "clj4rnx.pattern:clear()"))
  (doseq [_ (range (:track-cnt ctx))]
    (ev "clj4rnx.track = clj4rnx.song:insert_track_at(" 1 ")")
;    (ev "clj4rnx.track.visible_note_columns = 2")
    )
  (ev "clj4rnx.song.delete_track_at(" (:track-cnt ctx) ")")
  (doseq [_ (range (dec (count (:instrs ctx))))]
    (ev "clj4rnx.song:insert_instrument_at(" 1 ")"))
  (dorun (map-indexed (fn [idx m] (reset-instr (assoc m :instr-idx idx))) (:instrs ctx))))

(defn- note-col [t off-t off-vec]
;  (println "note-col:" t off-t off-vec)
  (let [index (or  (->> off-vec
                        (map-indexed vector)
                        (filter (fn [[index off-t]] (when (>= t off-t) index)))
                        ffirst)
                   (count off-vec))]
    [index (assoc off-vec index off-t)]))

(defn set-notes [trk-idx pitch-f notes]
;  (println "set-notes:" trk-idx notes)
  (ev "clj4rnx.track = clj4rnx.pattern:track(" trk-idx ")")
  (ev "clj4rnx.track:clear()")
  (let [off-vec (reduce (fn [off-vec {:keys [t deg oct v d]}]
                          (let [l (inc (int (* t 4 *lpb*)))
                                [note-col off-vec] (note-col t (+ t d) off-vec)
                                note-col (inc note-col)]
                            (ev "clj4rnx.note_column = clj4rnx.track:line(" l "):note_column(" note-col ")")
                            (ev "clj4rnx.note_column.note_value = " (pitch-f deg oct))
                            (ev "clj4rnx.note_column.volume_value = " (math/round (* v 254)))
                            (ev "clj4rnx.note_column.instrument_value = " (dec trk-idx))
                            (ev "clj4rnx.note_column = clj4rnx.track:line(" (int (+  l (* d 4 *lpb*))) "):note_column(" note-col ")")
                            (ev "clj4rnx.note_column.note_string = 'OFF'" )
                            off-vec))
                        [] notes)
        vis-note-cols (count off-vec)]
    (ev "if clj4rnx.song:track(" trk-idx ").visible_note_columns < " vis-note-cols
        " then clj4rnx.song:track(" trk-idx ").visible_note_columns = " vis-note-cols " end")))

(def note-re* #"([1-9]*)([+-]*)([whqes]*)")
; (map #(re-find note-re* %) ["1" "2+" "7--" "w"])

(defn- note-from-keyword [kw]
  (if-let [[n deg oct d] (re-find note-re* (apply str (rest (str kw))))]
    (let [; _ (println n)
          oct (reduce #(+ %1 (if (= %2 \+) 1 -1)) 0 oct)
          deg (if-not (= deg "") (Integer. deg))
          d-map {\w 1 \h 1/2 \q 1/4 \e 1/8 \s 1/12}
          d (if (= d "") 1/4 (reduce #(+ %1 (d-map %2)) 0 d))]
      (into {:d d} (if deg {:deg deg :oct oct :v 3/4})))
    (throw (Exception. (str "note-from-keyword: unable to parse " kw)))))

; (map #(note-from-keyword %) [:1e :e :+2 :-7s])
; (note-from-keyword :e)

(defn- notes-from-keyword [ctx kw]
;  (println "notes-from-keyword:" ctx kw)
  (let [n (assoc (note-from-keyword kw) :t (:t ctx))]
    (conj ctx [:t (+ (:t ctx) (:d n))] (when (:deg n) [:ns (conj (:ns ctx) n)]))))

(declare coll-to-notes)

(defn- set-to-notes [ctx coll]
;  (println "set-to-notes:" ctx coll)
  (let [new-ctx (reduce (fn [{max-t :max-t :as new-ctx} val]
                          (let [new-ctx (assoc new-ctx :t (:t ctx))
                                new-ctx (cond
                                         (vector? val) (coll-to-notes new-ctx val)
                                         (keyword? val) (notes-from-keyword new-ctx val)
                                         :else (throw (Exception. (str "from-set: unexpected val: " val))))]
                            (assoc new-ctx :max-t (max (:t new-ctx) (:max-t new-ctx) max-t))))
                        (assoc ctx :ns []  :max-t (:t ctx)) coll)]
;    (println "set-to-notes: new-ctx:" new-ctx)
    (assoc new-ctx :t (:max-t new-ctx) :ns (concat (:ns ctx) (sort #(compare (:t %1) (:t %2)) (:ns new-ctx))))))

(defn- coll-to-notes
  ([coll]
     (:ns (coll-to-notes {:t 0 :ns []} coll)))
  ([ctx coll]
;     (println "coll-to-notes:" ctx coll)
     (reduce (fn [ctx val]
               (cond
                (set? val) (set-to-notes ctx val)
                (keyword? val) (notes-from-keyword ctx val)
                :else (throw (Exception. (str "coll-to-notes: unexpected val: " val)))))
             ctx coll)))

(defn- notes-to-bars [notes]
  (reduce (fn [bars {t :t :as n}]
            (let [idx (int t)
                  bars (into bars (repeat (- (inc idx) (count bars)) []))]
              (assoc bars idx (conj (bars idx) (assoc n :t (- (n :t) idx))))))
          [] notes))

(defn- coll-to-bars [coll] (->> coll coll-to-notes notes-to-bars))
(defn- coll-to-inf-bars [coll] (->> coll coll-to-bars repeat (mapcat identity)))

(def bar-fs* (ref {}))
(defn add-bar-f [name f] (dosync (alter bar-fs* assoc name f)) nil)
;(add-bar-f :qtr #(coll-to-inf-bars [:1 :1 :1 :1]))
(defn get-bars [name] ((@bar-fs* name)))

(def patr-fs* (ref {}))
(defn add-patr-f [name f] (dosync (alter patr-fs* assoc name f)))
(defn get-patr-f [name] (@patr-fs* name))

(add-patr-f :grv-1-full (fn []
                          {:bd (coll-to-inf-bars [:1 :1 :1 :1 :1])
                           :sd (coll-to-inf-bars [:q :1 :q :1])
                           :hh-c (coll-to-inf-bars [:e :1e :e :1e :e :1e :e :1e])
                           :hc (coll-to-inf-bars [:1 :1 :1 :1e :1e :1 :1 :1 :1e :s :1s])
                           :bass (coll-to-inf-bars [:e :1+e :e :1+e :e :1+e :e :1+e
                                                         :e :5e :e :5e :e :5e :e :5e
                                                         :e :6e :e :6e :e :6e :e :6e
                                                         :e :4e :e :4e :e :4e :e :4e])
                           :hov (coll-to-inf-bars [#{:1q :5q}])
                           :pad (coll-to-inf-bars [#{:1w [:1+h :2+h]}
                                           #{:1w [:5h :6h]}
                                           #{:1w [:6h :5h]}
                                           #{:1w [:4h :5h]}])}))

; (loop-patr 0)

(def *patr-specs*
     [{:patr-f (fn [] (select-keys ((get-patr-f :grv-1-full)) [:bd])) :bar-cnt 1}
      {:patr-f (fn [] (select-keys ((get-patr-f :grv-1-full)) [:bd :hh-c]))  :bar-cnt 1}
      {:patr-f (fn [] (select-keys ((get-patr-f :grv-1-full)) [:bd :hh-c :sd])) :bar-cnt 1}
      {:patr-f (get-patr-f :grv-1-full) :bar-cnt 4} ])

(defn set-bars [trk-idx pitch-f bars]
  (set-notes trk-idx pitch-f
             (mapcat
              (fn [idx bar]
                (map #(assoc %1 :t (+ idx (:t %1))) bar))
              (iterate inc 0)  bars)))

(defn- deg-to-pitch
  ([deg oct]
     (deg-to-pitch 48 deg oct))
  ([base deg oct]
      (+ base ([0 2 4 5 7 9 11] (dec deg)) (* oct 12))))

(defn set-patr- [idx patr-f bar-cnt]
;  (println "set-patr:" name idx)
  (ev "clj4rnx.pattern = clj4rnx.song:pattern(" (inc idx) ")")
  (ev "clj4rnx.pattern.number_of_lines = " (* bar-cnt 4 *lpb*))
  (let [patr (patr-f)]
    (set-bars 1 deg-to-pitch (take bar-cnt (:bd patr)))
    (set-bars 2 deg-to-pitch (take bar-cnt (:sd patr)))
    (set-bars 3 deg-to-pitch (take bar-cnt (:hh-c patr)))
    (set-bars 5 deg-to-pitch (take bar-cnt (:hc patr)))
    (set-bars 6 deg-to-pitch (take bar-cnt (:bass patr)))
    (set-bars 7 deg-to-pitch (take bar-cnt (:hov patr)))
    (set-bars 8 deg-to-pitch (take bar-cnt (:pad patr)))))

(defn set-patr [{:keys [patr-f bar-cnt]} idx]
  (ev "clj4rnx.pattern = clj4rnx.song:pattern(" (inc idx) ")")
  (ev "clj4rnx.pattern.number_of_lines = " (* bar-cnt 4 *lpb*))
  (let [patr (patr-f)]
    (set-bars 1 deg-to-pitch (take bar-cnt (:bd patr)))
    (set-bars 2 deg-to-pitch (take bar-cnt (:sd patr)))
    (set-bars 3 deg-to-pitch (take bar-cnt (:hh-c patr)))
    (set-bars 5 deg-to-pitch (take bar-cnt (:hc patr)))
    (set-bars 6 deg-to-pitch (take bar-cnt (:bass patr)))
    (set-bars 7 deg-to-pitch (take bar-cnt (:hov patr)))
    (set-bars 8 deg-to-pitch (take bar-cnt (:pad patr)))))

(defn loop-patr [idx]
  (set-patr (*patr-specs* idx) idx)
  (ev "clj4rnx.song.selected_sequence_index = " (inc idx))
  (ev "clj4rnx.song.transport.loop_pattern = true")
  (ev "clj4rnx.song.transport:start(renoise.Transport.PLAYMODE_CONTINUE_PATTERN)"))

(defn- set-patrs [specs]
  (map-indexed #(set-patr %2 %1) specs))

(defn demo []
  (reset-song {:bpm 140 :patr-cnt (count *patr-specs*) :track-cnt 7
               :instrs [{:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Bassdrums/VEC1 Trancy/VEC1 BD Trancy 10.wav"}
                        {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Snares/VEC1 Snare 031.wav"}
                        {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Cymbals/VEC1 Close HH/VEC1 Cymbals  CH 11.wav"}
                        {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Cymbals/VEC1 Open HH/VEC1 Cymbals  OH 001.wav"}
                        {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Claps/VEC1 Clap 027.wav"}
                        {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 87}
                        {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 83}
                        {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 29}]})
  (set-patrs *patr-specs*))

; (demo)

(defonce *interactive* false)
(when *interactive* (loop-patr 0))
