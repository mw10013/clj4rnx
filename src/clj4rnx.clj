(ns clj4rnx
  (:require [osc :as osc]))

(osc/osc-debug true)

(def *lpb* 4)
(defonce *client* (osc/osc-client "127.0.0.1" 8000))

(defn ev [& args]
;  (println "ev: " args)
  (osc/osc-send *client* "/renoise/evaluate" (apply str args)))

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
;      (Thread/sleep 1000)
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

