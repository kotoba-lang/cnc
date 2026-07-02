(ns kotoba.cam.toolpath
  "Toolpath generation: CAM operations, segment types, and job execution.
   Ported from kami-cam (Rust) `src/toolpath.rs`.

   A `CamOperation` is a plain tagged map `{:op :pocket|:drill|... ...}`:
     {:op :face-mill  :tool-id :depth-of-cut :stepover :feed-rate :spindle-rpm}
     {:op :pocket     :tool-id :depth :stepover :strategy :feed-rate :spindle-rpm
                       :pocket-min :pocket-max}     ; corners are vec3
     {:op :contour    :tool-id :depth :side :feed-rate :spindle-rpm}
     {:op :drill      :tool-id :depth :peck-depth :feed-rate :spindle-rpm :holes}
     {:op :surface-3d :tool-id :stepover :strategy :feed-rate :spindle-rpm}
     {:op :turn       :tool-id :depth-of-cut :feed-rate :spindle-rpm}

   Only `:pocket` (zigzag) and `:drill` (peck cycles) are actually simulated;
   the rest generate a placeholder rapid move to the tool-change point, same
   as the Rust original — a real implementation would need real geometry
   input (surface meshes, contour polylines) which this crate never had."
  (:require [kotoba.cam.vec3 :as vec3]
            [kotoba.cam.util :as util]))

(def pocket-strategies #{:zigzag :spiral :trochoidal-peel})
(def surface-strategies #{:raster :spiral :waterline :pencil})
(def contour-sides #{:inside :outside :on-line})
(def segment-types #{:rapid :linear :arc-cw :arc-ccw})

(defn segment
  "One segment of a generated toolpath."
  [{:keys [segment-type start end feed-rate center tool-id]}]
  {:segment-type segment-type
   :start start
   :end end
   :feed-rate (double (or feed-rate 0.0))
   :center center
   :tool-id tool-id})

(defn- last-end
  "The end position of the last segment, or origin if empty."
  [segments]
  (if (seq segments) (:end (peek segments)) vec3/zero))

;; ---------------------------------------------------------------------
;; :pocket — zigzag pocket clearing
;; ---------------------------------------------------------------------

(defn- gen-pocket
  [job {:keys [tool-id depth stepover feed-rate pocket-min pocket-max]} segments0]
  (let [tool (get (:tool-library job) tool-id)
        tool-radius (if tool (/ (:diameter tool) 2.0) 0.0)
        effective-stepover (if (> stepover 0.0) stepover tool-radius)
        x-min (+ (:x pocket-min) tool-radius)
        x-max (- (:x pocket-max) tool-radius)
        y-min (+ (:y pocket-min) tool-radius)
        y-max (- (:y pocket-max) tool-radius)
        z-top (:z pocket-min)
        z-bottom (- z-top depth)
        safe-z (+ z-top (:safe-height job))
        ;; Simple constant depth-of-cut = stepover for now; a real
        ;; implementation would use axial DOC (ported verbatim from Rust).
        layer-doc (min effective-stepover depth)
        num-layers (long (util/ceil (/ depth layer-doc)))]
    (loop [layer 0 segments segments0]
      (if (>= layer num-layers)
        segments
        (let [z (max (- z-top (* (+ layer 1.0) layer-doc)) z-bottom)
              first-start (vec3/v3 x-min y-min safe-z)
              segments (if (seq segments)
                         (conj segments
                               (segment {:segment-type :rapid
                                         :start (last-end segments)
                                         :end (vec3/v3 x-min y-min safe-z)
                                         :tool-id tool-id}))
                         segments)
              segments (conj segments
                             (segment {:segment-type :rapid
                                       :start first-start
                                       :end (vec3/v3 x-min y-min z)
                                       :tool-id tool-id}))
              segments (loop [y y-min forward true segments segments]
                         (if (> y y-max)
                           segments
                           (let [[sx ex] (if forward [x-min x-max] [x-max x-min])
                                 start (vec3/v3 sx y z)
                                 end-pt (vec3/v3 ex y z)
                                 prev (last-end segments)
                                 segments (if (> (vec3/length (vec3/sub prev start)) 1e-6)
                                            (conj segments
                                                  (segment {:segment-type :rapid
                                                            :start prev :end start
                                                            :tool-id tool-id}))
                                            segments)
                                 segments (conj segments
                                                (segment {:segment-type :linear
                                                          :start start :end end-pt
                                                          :feed-rate feed-rate
                                                          :tool-id tool-id}))]
                             (recur (+ y effective-stepover) (not forward) segments))))
              prev (last-end segments)
              segments (conj segments
                             (segment {:segment-type :rapid
                                       :start prev
                                       :end (vec3/v3 (:x prev) (:y prev) safe-z)
                                       :tool-id tool-id}))]
          (recur (inc layer) segments))))))

;; ---------------------------------------------------------------------
;; :drill — peck-drill cycles
;; ---------------------------------------------------------------------

(defn- gen-drill
  [job {:keys [tool-id depth peck-depth feed-rate holes]} segments0]
  (let [safe-z (:safe-height job)]
    (reduce
     (fn [segments hole]
       (let [top (vec3/v3 (:x hole) (:y hole) safe-z)
             prev (last-end segments)
             segments (conj segments
                            (segment {:segment-type :rapid :start prev :end top
                                      :tool-id tool-id}))
             z-bottom (- (:z hole) depth)]
         (loop [z (:z hole) segments segments]
           (if (<= z z-bottom)
             segments
             (let [target-z (max (- z peck-depth) z-bottom)
                   segments (conj segments
                                  (segment {:segment-type :linear
                                            :start (vec3/v3 (:x hole) (:y hole) z)
                                            :end (vec3/v3 (:x hole) (:y hole) target-z)
                                            :feed-rate feed-rate
                                            :tool-id tool-id}))
                   segments (conj segments
                                  (segment {:segment-type :rapid
                                            :start (vec3/v3 (:x hole) (:y hole) target-z)
                                            :end top
                                            :tool-id tool-id}))]
               (recur target-z segments))))))
     segments0
     holes)))

;; ---------------------------------------------------------------------
;; placeholder ops — face-mill / contour / surface-3d / turn
;; ---------------------------------------------------------------------

(defn- gen-placeholder
  [job {:keys [tool-id]} segments]
  (let [prev (last-end segments)]
    (conj segments
          (segment {:segment-type :rapid :start prev
                    :end (vec3/v3 0.0 0.0 (:safe-height job))
                    :tool-id tool-id}))))

;; ---------------------------------------------------------------------
;; CamJob
;; ---------------------------------------------------------------------

(defn new-job
  "A complete CAM job combining stock, tool library, and ordered operations."
  [stock tool-library]
  {:stock stock :operations [] :tool-library tool-library :safe-height 5.0})

(defn add-operation [job op] (update job :operations conj op))

(defn generate-toolpath
  "Generate toolpath segments for all operations in order. Currently
   implements zigzag pocket + peck drill; other operations produce
   placeholder rapid moves to the operation location so the G-code
   structure is valid (ported verbatim from Rust)."
  [job]
  (reduce
   (fn [segments op]
     (case (:op op)
       :pocket (gen-pocket job op segments)
       :drill (gen-drill job op segments)
       (:face-mill :contour :surface-3d :turn) (gen-placeholder job op segments)))
   []
   (:operations job)))
