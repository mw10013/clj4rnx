(ns clj4rnx
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
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

  (do
    (ev "clj4rnx.instrument = renoise.song():instrument(1)")
    (ev "clj4rnx.instrument:clear()")
    (ev "clj4rnx.instrument.plugin_properties:load_plugin('')")
    (ev "clj4rnx.instrument.name = 'instr'")
    (ev "clj4rnx.sample = clj4rnx.instrument:sample(1)")
    (ev "clj4rnx.sample_buffer = clj4rnx.sample.sample_buffer")
    (ev "clj4rnx.sample_buffer:load_from('/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Bassdrums/VEC1 Clubby/VEC1 BD Clubby 01.wav')")
    )
  )

(defn reset-song [ctx]
  (ev "clj4rnx.song = renoise.song()")
  (ev "clj4rnx.sequencer = clj4rnx.song.sequencer")
  (doseq [_ (range 25)]
    (ev "clj4rnx.sequencer:delete_sequence_at(" 1 ")")
    (ev "clj4rnx.song:delete_track_at(" 1 ")")
    (ev "clj4rnx.song:delete_instrument_at(" 1 ")"))
  
  (doseq [idx (range 2 (inc (:patr-cnt ctx)))]
       (ev "clj4rnx.sequencer:insert_sequence_at(" idx ", " idx ")"))
  (doseq [_ (range (dec (:track-cnt ctx)))]
    (ev "clj4rnx.song:insert_track_at(" 1 ")"))
  (doseq [_ (range (dec (:instr-cnt ctx)))]
    (ev "clj4rnx.song:insert_instrument_at(" 1 ")"))
  (map-indexed (fn [idx m] (reset-instr (assoc m :instr-idx idx))) (:instrs ctx)))

(comment
  (reset-song {:patr-cnt 3 :track-cnt 2 :instr-cnt 2
               :instrs [{:plugin-name "Audio/Generators/VST/Sylenth1" :preset 82}
                        {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 83}]})
  (reset-song {:patr-cnt 4 :track-cnt 10 :instr-cnt 5
               :instrs [{:plugin-name "Audio/Generators/VST/Sylenth1" :preset 42}
                        {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 43}]})
  (reset-song 5)
  )

(defn reset-patr [idx beat-cnt]
  (ev "clj4rnx.pattern = renoise.song():pattern(" idx ")")
  (ev "clj4rnx.pattern.number_of_lines = " (* beat-cnt *lpb*))
;  (ev "clj4rnx.pattern:clear()")
  )

(defn set-notes [trk-idx notes]
  (ev "clj4rnx.track = clj4rnx.pattern:track(" trk-idx ")")
  (ev "clj4rnx.track:clear()")
  (doseq [[t p v d] notes]
    (let [l (inc (* t *lpb*))]
      (ev "clj4rnx.note_column = clj4rnx.track:line(" l "):note_column(1)")
      (ev "clj4rnx.note_column.note_value = " p)
      (ev "clj4rnx.note_column.volume_value = " v)
      (ev "clj4rnx.note_column = clj4rnx.track:line(" (int (+  l (* d *lpb*))) "):note_column(1)")
      (ev "clj4rnx.note_column.note_value = " 120)
      )))

(defn set-notes- [trk-idx notes]
  (let [trk (str "renoise.song():pattern(1):track(" trk-idx ")")]
    (ev trk ":clear()")
    (doseq [[t p v d] notes]
      (let [l (inc (* t *lpb*))
            nc (str trk ":line(" l "):note_column(1)")
            nc-off (str trk ":line(" (int (+  l (* d *lpb*))) "):note_column(1)")]
        (ev nc ".note_value = " p)
        (ev nc ".volume_value = " v)
        (ev nc-off ".note_value = " 120)))))

(comment
  (set-notes 1 [[0 60 100 0.5]])
  (set-notes 1 [[0 66 90 0.5]])
  (set-notes 1 [[0 60 80 1] [1 62 85 1]])
  (set-notes 1 [[0 70 80 1] [1 72 85 1] [2 60 80 1] [3 62 85 1]])
  (set-notes 1 [[0 40 80 1] [1 42 85 1] [2 43 80 1] [3 44 85 1] [4 45 85 1]])
  
  (reset-patr 1 4)
  (reset-patr 1 8)
  
  (osc/osc-send *client* "/renoise/evaluate" "clj4rnx.track = renoise.song().patterns[1]:track(1)")
  (osc/osc-send *client* "/renoise/evaluate" "clj4rnx.line = clj4rnx.track:line(1)")
  (osc/osc-send *client* "/renoise/evaluate" "clj4rnx.line:note_column(1).note_string = 'C-5'")
  (osc/osc-send *client* "/renoise/evaluate" "clj4rnx.line:note_column(1).note_string = 'D-6'")

  (osc/osc-send *client* "/renoise/song/bpm" 120)
  (osc/osc-send *client* "/renoise/song/bpm" 140)
  (osc/osc-send *client* "/renoise/evaluate" "renoise.song().transport.bpm = 234")
  )

