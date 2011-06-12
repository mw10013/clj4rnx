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
      (ev "clj4rnx.plugin_properties.plugin_device.active_preset = " (:preset ctx)))))

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

(defn set-notes [trk-idx notes]
  (ev "clj4rnx.track = clj4rnx.pattern:track(" trk-idx ")")
  (ev "clj4rnx.track:clear()")
  (doseq [{:keys [t p v d]} notes]
    (let [l (inc (int (* t 4 *lpb*)))]
      (ev "clj4rnx.note_column = clj4rnx.track:line(" l "):note_column(1)")
      (ev "clj4rnx.note_column.note_value = " p)
      (ev "clj4rnx.note_column.volume_value = " (math/round (* v 254)))
      (ev "clj4rnx.note_column.instrument_value = " (dec trk-idx))
      (ev "clj4rnx.note_column = clj4rnx.track:line(" (int (+  l (* d *lpb*))) "):note_column(1)")
      (ev "clj4rnx.note_column.note_string = 'OFF'" ))))

(defn set-bars [trk-idx bars]
  (println "set-bars:" bars)
  (set-notes
   trk-idx
   (mapcat
    (fn [idx bar]
      (map #(assoc %1 :t (+ idx (:t %1))) bar))
    (iterate inc 0)  bars)))

(def n*
     {
      :c4w {:p 48 :v 3/4 :d 1}
      :c4h {:p 48 :v 3/4 :d 1/2}
      :c4q {:p 48 :v 3/4 :d 1/4}
      :c4e {:p 48 :v 3/4 :d 1/8}
      :c4s {:p 48 :v 3/4 :d 1/16}
      :d4q {:p 50 :v 3/4 :d 1/4}
      :d4e {:p 50 :v 3/4 :d 1/8}
      :e4q {:p 52 :v 3/4 :d 1/4}
      })

;(map #(re-find #"(([cdefgab])(\d*))?([whqis]*)" %) ["c4" "w" "i" "d4" "e4h"]) 

(defn- note-from-keyword [kw]
  (if-let [[_ p? p oct d] (re-find #"(([cdefgab])(\d*))?([whqis]*)" (apply str (rest (str kw))))]
    (let [p-map {"c" 0 "d" 2 "e" 4 "f" 5 "g" 7 "a" 9 "b" 11}
          p (when p? (+ (* (Integer. (if (= oct "") 4 (Integer. oct))) 12) (p-map p)))
          d-map {\w 1 \h 1/2 \q 1/4 \i 1/8 \s 1/12}
          d (if (= d "") 1/4 (reduce #(+ %1 (d-map %2)) 0 d))]
      {:p p :v 3/4 :d d})
    (throw (Exception. (str "note-from-keyword: unable to parse " kw)))))

; (map #(note-from-keyword %) [:c4 :i :d4])
; (note-from-keyword :d)

(defn- notes-from-keyword [ctx kw]
  (println "notes-from-keyword:" ctx kw)
  (let [n (assoc (note-from-keyword kw) :t (:t ctx))]
    (conj ctx [:t (+ (:t ctx) (:d n))] (when (:p n) [:ns (conj (:ns ctx) n)]))))

(notes-from-keyword {:t 0 :ns [{:n 0}]} :i)

(defn- set-to-notes [ctx coll]
  (println "set-to-notes:" ctx coll)
  (let [new-ctx (reduce (fn [{max-t :max-t :as new-ctx} val]
                          (let [new-ctx (assoc new-ctx :t (:t ctx))
                                new-ctx (cond
                                         (vector? val) (from-coll new-ctx val)
                                         (keyword? val) (notes-from-keyword new-ctx val)
                                         :else (throw (Exception. (str "from-set: unexpected val: " val))))]
                            (assoc new-ctx :max-t (max (:t new-ctx) (:max-t new-ctx) max-t))))
                        (assoc ctx :ns []  :max-t (:t ctx)) coll)]
    (println "set-to-notes: new-ctx:" new-ctx)
    (assoc new-ctx :t (:max-t new-ctx) :ns (concat (:ns ctx) (sort #(compare (:t %1) (:t %2)) (:ns new-ctx))))))

(defn- coll-to-notes
  ([coll]
     (:ns (coll-to-notes {:t 0 :ns []} coll)))
  ([ctx coll]
     (println "coll-to-notes:" ctx coll)
     (reduce (fn [ctx val]
               (cond
                (set? val) (set-to-notes ctx val)
                (keyword? val) (notes-from-keyword ctx val)
                :else (throw (Exception. (str "from-coll: unexpected val: " val)))))
             ctx coll)))

(defn- notes-to-bars [notes]
  (reduce (fn [bars {t :t :as n}]
            (let [idx (int t)
                  bars (into bars (repeat (- (inc idx) (count bars)) []))]
              (assoc bars idx (conj (bars idx) (assoc n :t (- (n :t) idx))))))
          [] notes))

(defn- coll-to-bars [coll] (->> coll coll-to-notes notes-to-bars))

; (coll-to-bars [:c4w :c4w :c4e :c4e])

; (coll-to-notes [:c4w :c4w :c4e :c4e])
; (coll-to-notes [:i :ci :i :ci])
; (coll-to-notes [#{:c4h [:d4q :e4q :c4q]}])
; (coll-to-notes [#{:c4h :d4q}])
; (coll-to-notes [#{[:c4s :c4s :c4s] [:d4e :d4e]}])
; (def c* (coll-to-notes [:c4w #{[:c4s :c4s :c4s] [:d4e :d4e]}]))
; (notes-to-bars (coll-to-notes [:c4w :c4w :c4e :c4e]))
; (notes-to-bars (coll-to-notes [:c4w #{[:c4s :c4s :c4s] [:d4e :d4e]}]))¯

(comment (defn repeat-beats [n-coll dur & args]
           (let [times (if (first args) (range (first args)) (iterate inc 0))]
             (mapcat (fn [t] (map #(alter-note %1 1 + %2) n-coll (repeat (* dur t)))) times))))

(defn take-beats [n n-coll]  (take-while #(< (% 0) n) n-coll))

(defn assoc-time- [t-coll dur n-coll]
  (map #(assoc %1 0 %2) n-coll (mapcat #(map + t-coll (repeat (* dur %))) (iterate inc 0))))
; (->> *base-note* repeat (assoc-time [0.0] 0.5) (take 5))
; (take 5 (assoc-time [0.0] 1.0 (repeat *base-note*)))

(def assoc-time (partial map #(assoc %2 0 %1))) ; [t-coll n-coll]
(def assoc-pitch (partial map #(assoc %2 1 %1))) ; [p-coll n-coll]
(def assoc-vel (partial map #(assoc %2 2 %1))) ; [vel-coll n-coll]
(def assoc-dur (partial map #(assoc %2 3 %1))) ; [dur-coll n-coll]
(def assoc-tp (partial map (fn [tp n] (-> n (assoc 0 (tp 0)) (assoc 1 (tp 1)))))) ; [tp-coll n-coll]

(defn assoc-bars [bars]
  (mapcat 
   (fn [idx bar]
     (map #(assoc %1 0 (+ (* 4 idx) (%1 0))) bar))
   (iterate inc 0) (cycle bars)))

; (take 7 (assoc-bars [[[0 48] [3/2 48]] [[0 46] [3/2 46]]]))

; [[[0 48] [3/2 48]] [[0 46] [3/2 46]]]
; (take 5 (map-indexed #(vector %1 %2) [[[0 48] [3/2 48]] [[0 46] [3/2 46]]]))
; (take 5 (map-indexed #(assoc %2 0 %1) [[[0 48] [3/2 48]] [[0 46] [3/2]46]]]))

(defn within-beats [l u & args]
  (let [len (if (odd? (count args)) (last args))
        coll (concat [[l u]] (partition 2 args))]
    (fn [n]
      (let [v (if len (mod (n 0) len) (n 0))]
        (println "n: " n)
        (some #(and (<= (first %) v) (<= v (last %))) coll)))))

;  (take 8 (filter (within-beats 1/8 2) [[0 0] [1/8 1/8] [1/4 1/4] [2 2] [3 3]]))

(def note-fs* (ref {}))
(defn add-note-f [name f] (dosync (alter note-fs* assoc name f)) nil)
(defn get-notes [name] ((@note-fs* name)))
(defn take-notes [name beat-cnt] (->> name get-notes (take-beats beat-cnt)))

(add-note-f :qtr #(->> [0 48 3/4 1] repeat (assoc-time (iterate inc 0))))
(add-note-f :six #(->> (get-notes :qtr) (assoc-time (iterate (partial + 1/4) 0)) (assoc-dur (repeat 1/4))))
(add-note-f :off-eighths #(->> (get-notes :qtr) (assoc-time (iterate inc 1/2)) (assoc-dur (repeat 1/4))))
(add-note-f :one-three #(->> (get-notes :qtr) (assoc-time (iterate (partial + 2) 1))))
(add-note-f :bass-1 #(->> (get-notes :off-eighths) (assoc-pitch (cycle [48 48 50 50 52 52 55 55]))))
(add-note-f :bass-1a #(->> (get-notes :off-eighths) (assoc-tp [[0 48] [1/4 46] [1 48] [5/4 48]])))

(def bar-fs* (ref {}))
(defn add-bar-f [name f] (dosync (alter bar-fs* assoc name f)) nil)
(defn get-bars [name] ((@bar-fs* name)))

; what about dur.
(def *base-note* [0 48 3/4 1])
(add-bar-f :whl #(repeat [*base-note*]))
;(add-bar-f :qtr #(repeat (map (fn [b] (assoc *base-note* 0 b)) (range 4))))
(add-bar-f :qtr #(mapcat identity (repeat (coll-to-bars [:i :ci :i :ei :f]))))
(add-bar-f :eth #(repeat (map (fn [b] (assoc *base-note* 0 (* b 1/2))) (range 8))))
(add-bar-f :off-eth #(repeat (map (fn [b] (assoc *base-note* 0 (+ b 1/2))) (range 4))))
(add-bar-f :six #(repeat (map (fn [b] (assoc *base-note* 0 (* b 1/4))) (range 16))))
(add-bar-f :bass-1 #(cycle [[[0 48 3/4 1] [3/2 48 3/4 1/2]]]))

; (set-patr :grv-1 0 2)
; (take 2 (get-bars :bass-1))

(def patr-fs* (ref {}))
(defn add-patr-f [name f] (dosync (alter patr-fs* assoc name f)))
(defn get-patr [name] ((@patr-fs* name)))

(comment (add-patr-f :grv-1 (fn []
                              {:bd (get-bars :whl)
                               :sd (get-bars :qtr)
                               :hh-c (get-bars :off-eth)
                               :bass (get-bars :bass-1)
                               })))

(add-patr-f :grv-1 (fn []
                       {:bd (get-bars :qtr)
;                        :sd (get-bars :qtr)
;                        :hh-c (get-bars :off-eth)
;                        :bass (get-bars :bass-1)
                        }))

(add-patr-f :grv-0 (fn []
                     (assoc {} :bd (:bd (get-patr :grv-1)))
                     ))

; (set-patrs (subvec patr-specs* 1 2))
; (set-patr :grv-1 0 2)
(defn set-patr [name idx bar-cnt]
  (ev "clj4rnx.pattern = clj4rnx.song:pattern(" (inc idx) ")")
  (ev "clj4rnx.pattern.number_of_lines = " (* bar-cnt 4 *lpb*))
  (let [patr (get-patr name)]
    (set-bars 1 (take bar-cnt (:bd patr)))
;    (set-bars 2 (take bar-cnt (:sd patr)))
;    (set-bars 3 (take bar-cnt (:hh-c patr)))
;    (set-bars 5 (take bar-cnt (:bass patr)))
    ))

(def patr-specs* [{:patr :grv-0 :bar-cnt 2} {:patr :grv-1 :bar-cnt 2}])

(defn- set-patrs [specs]
  (map-indexed #(set-patr (:patr %2) %1 (:bar-cnt %2)) specs))

(defn demo []
  (reset-song {:patr-cnt 3 :track-cnt 7
               :instrs [{:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Bassdrums/VEC1 Trancy/VEC1 BD Trancy 10.wav"}
                        {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Snares/VEC1 Snare 031.wav"}
                        {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Cymbals/VEC1 Close HH/VEC1 Cymbals  CH 11.wav"}
                        {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Cymbals/VEC1 Open HH/VEC1 Cymbals  OH 001.wav"}
                        {:sample-filename "/Users/mw/Documents/music/vengence/VENGEANCE ESSENTIAL CLUB SOUNDS vol-1/VEC1 Claps/VEC1 Clap 027.wav"}
                        {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 87}
                        {:plugin-name "Audio/Generators/VST/Sylenth1" :preset 83}]})
                                        ;  (set-patr-0 4)
  (set-patrs patr-specs*)
  )
; (demo)

(def k*
     {
      :1w "1w"
      :1h"1h"
      :1q "1q"
      :1e "1e"
      :5w "5w"
      :5h"5h"
      :5q "5q"
      :5e "5e"
      :-1w "-1w"
      :-1h"-1h"
      :-1q "-1q"
      :-1e "-1e"
      })

[:1q :1q :1q :1q]
['(ch :-1w :1w :5q) :6q]
['(ch :-1w :1w :5e) :6q :2e]
['(ch :-1w :1q :5e) '(ch :6q :0e) :2e]
; sometimes vertical, sometimes horizontal
; jfugue uses + to indicate vertical, - to indicate ties
[:-1w+1q+5e ]


; :c4www.v3:4
(map #(re-find #"([cdefgab])(\d*)([whqes]*)" %) ["c4wwww" "c" "cw" "cwwwq"])
