(ns clj4rnx
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
      (ev "clj4rnx.plugin_properties.plugin_device.active_preset = " (:preset ctx))))
)

(comment
  (reset-instr {:instr-idx 0 :plugin-name "Audio/Generators/VST/Sylenth1" :preset 82})
  (reset-instr {:instr-idx 0 :plugin-name "Audio/Generators/VST/Sylenth1" :preset 82
                :sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Bassdrums/VEC1 Clubby/VEC1 BD Clubby 01.wav"})
  (reset-instr {:instr-idx 0 
                :sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Bassdrums/VEC1 Clubby/VEC1 BD Clubby 05.wav"})
  (reset-instr {:instr-idx 0 :plugin-name "Audio/Generators/VST/Sylenth1" :preset 83})
  (reset-instr {:instr-idx 0 :plugin-name "Audio/Generators/VST/Vanguard"})
  )

(defn reset-song [ctx]
  (ev "clj4rnx.song = renoise.song()")
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
  (doseq [_ (range (dec (:track-cnt ctx)))]
    (ev "clj4rnx.song:insert_track_at(" 1 ")"))
  (doseq [_ (range (dec (count (:instrs ctx))))]
    (ev "clj4rnx.song:insert_instrument_at(" 1 ")"))
  (dorun (map-indexed (fn [idx m] (reset-instr (assoc m :instr-idx idx))) (:instrs ctx))))

(comment
  (reset-song {:patr-cnt 3 :track-cnt 7
               :instrs [{:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Bassdrums/VEC1 Trancy/VEC1 BD Trancy 10.wav"}
                        {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Snares/VEC1 Snare 031.wav"}
                        {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Cymbals/VEC1 Close HH/VEC1 Cymbals  CH 11.wav"}
                        {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Cymbals/VEC1 Open HH/VEC1 Cymbals  OH 001.wav"}
                        {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Claps/VEC1 Clap 027.wav"}
                        {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 82}
                        {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 83}]})
  
  (reset-song {:patr-cnt 3 :track-cnt 2 :instr-cnt 2
               :instrs [{:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Bassdrums/VEC1 Clubby/VEC1 BD Clubby 05.wav"}
                        {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 82}
                        {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 83}]})
  )

(defn set-patr [idx beat-cnt]
  (ev "clj4rnx.pattern = clj4rnx.song:pattern(" (inc idx) ")")
  (ev "clj4rnx.pattern.number_of_lines = " (* beat-cnt *lpb*)))

(defn set-notes [trk-idx notes]
  (ev "clj4rnx.track = clj4rnx.pattern:track(" trk-idx ")")
  (ev "clj4rnx.track:clear()")
  (doseq [[t p v d] notes]
    (let [l (inc (* t *lpb*))]
      (ev "clj4rnx.note_column = clj4rnx.track:line(" l "):note_column(1)")
      (ev "clj4rnx.note_column.note_value = " p)
      (ev "clj4rnx.note_column.volume_value = " (math/round (* v 254)))
      (ev "clj4rnx.note_column.instrument_value = " (dec trk-idx))
      (ev "clj4rnx.note_column = clj4rnx.track:line(" (int (+  l (* d *lpb*))) "):note_column(1)")
      (ev "clj4rnx.note_column.note_string = 'OFF'" ))))

; (defn xform-note [n key f & args] (assoc n key (apply f (n key) args)))

(comment (defn repeat-beats [n-coll dur & args]
           (let [times (if (first args) (range (first args)) (iterate inc 0))]
             (mapcat (fn [t] (map #(alter-note %1 1 + %2) n-coll (repeat (* dur t)))) times))))

(defn take-beats [n coll]  (take-while #(< (% 0) n) coll))

(defn xform-time [n t-coll dur]
  (map #(assoc %1 0 %2) (repeat n) (mapcat #(map + t-coll (repeat (* dur %))) (iterate inc 0))))

(defn within-beats [l u & args]
  (let [len (if (odd? (count args)) (last args))
        coll (concat [[l u]] (partition 2 args))]
    (fn [n]
      (let [v (if len (mod (n 0) len) (n 0))]
        (some #(and (<= (first %) v) (<= v (last %))) coll)))))

(defn- set-patr-0 []
  (set-patr 0 4)
  (set-notes 1 (take-beats 8.0 (xform-time [0 48 0.8 0.5] [0.0] 1.0))))

(defn- set-patr-1 []
  (set-patr 1 4)
  (set-notes 1 (take-beats 8.0 (xform-time [0 48 0.8 0.5] [0.0] 1.0)))
  (set-notes 2 (take-beats 8.0 (xform-time [0 48 0.8 0.5] [1.0] 2.0))))

(defn- set-patr-2 []
  (set-patr 2 4)
  (set-notes 1 (take-beats 8.0 (xform-time [0 48 0.8 0.5] [0.0] 1.0)))
  (set-notes 2 (take-beats 8.0 (xform-time [0 48 0.8 0.5] [1.0] 2.0)))
  (set-notes 3 (take-beats 8.0 (xform-time [0 48 0.8 0.25] [0.5] 1.0))))


(defn demo []
  (reset-song {:patr-cnt 3 :track-cnt 7
               :instrs [{:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Bassdrums/VEC1 Trancy/VEC1 BD Trancy 10.wav"}
                        {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Snares/VEC1 Snare 031.wav"}
                        {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Cymbals/VEC1 Close HH/VEC1 Cymbals  CH 11.wav"}
                        {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Cymbals/VEC1 Open HH/VEC1 Cymbals  OH 001.wav"}
                        {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Claps/VEC1 Clap 027.wav"}
                        {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 82}
                        {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 83}]})
  (set-patr-0)
  (set-patr-1)
  (set-patr-2)
  )
; (demo)

(comment
  (demo)
  
  (set-patr 0 4)
  (set-patr 0 8)
  (set-notes 1 [[0 48 0.8 0.5]])
  
  (set-notes 1 (take-beats 8.0 (xform-time [0 48 0.8 0.5] [0.0] 1.0)))
  (set-notes 2 (take-beats 8.0 (xform-time [0 48 0.8 0.5] [1.0] 2.0)))
  (set-notes 3 (take-beats 8.0 (xform-time [0 48 0.8 0.25] [0.5] 1.0)))
  
  (set-notes 1 (take-beats 4.0 (filter (within-beats 0.0 1.0 4.0) (xform-time [0 48 0.8 0.5] [0.0] 1.0))))
  (set-notes 1 (take-beats 4.0 (filter (within-beats 0.0 0.5 3.0 3.5 4.0) (xform-time [0 48 0.8 0.5] [0.0] 1.0))))
  (set-notes 1 [[1 48 0.8 0.5]])
  (set-notes 1 [[0 60 0.8 1] [1 62 0.85 1]])
  (set-notes 1 [[0 70 0.8 1] [1 72 0.85 1] [2 60 0.8 1] [3 62 0.85 1]])
  (set-notes 1 [[0 40 0.8 1] [1 42 0.85 1] [2 43 0.8 1] [3 44 0.85 1] [4 45 0.85 1]])
  )

