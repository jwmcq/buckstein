(ns clj-scad.buckycup
  (:require [scad-clj.model :refer :all]
            [scad-clj.scad :refer [write-scad]]))

(defmacro dimensions [& dims]
  (let [pairs (partition 2 dims)]
    `(do ~@(map (fn [[s v]] (list 'def (symbol s) v)) pairs))))

(dimensions bottle-width 80
            height 120
            base-thickness 10
            wall-thickness 2
            cutout-width 7
            cutout-height 15
            text-size 18
            handle-rad 12
            handle-extension 80
            window-offset (/ pi 16))

(def window-cutout
  (let [ext (+ (/ bottle-width 2) (* 4 wall-thickness))
        h (* 2 height)]
    (rotate [0 0 (/ pi 4)]
        (translate [0 0 base-thickness]
                   (intersection
                    (translate [0 0 (/ h 2)]
                               (cylinder ext h))
                    (translate [(/ h 2) (/ h 2) (/ h 2)]
                               (cube h h h)))))))

(def actual-window-cutout
  (union
   window-cutout
   (rotate [0 0 window-offset]
           window-cutout)
   (rotate [0 0 (- window-offset)]
           window-cutout)))

(def cutout
  (let [h (* 2 height)]
    (union actual-window-cutout
           (translate [0 0 (+ (/ h 2) base-thickness)]
             (cylinder (/ bottle-width 2) h)))))
    

(def main-shell
  (let [h (+ height (* 2 wall-thickness))]
    (difference
     (translate [0 0 (/ h 2)]
                (cylinder (+ (* 2 wall-thickness) (/ bottle-width 2)) h))
     cutout)))
    
(def handle
  (let [support-thickness (* 4 wall-thickness)
        support-breadth (* handle-rad 1.5)]
    (difference
     (union
      (translate [0 (- handle-extension) (/ height 2)]
                 (cylinder handle-rad height))
      (translate [0 (- (/ handle-extension 2)) (/ support-thickness 2)]
                 (cube support-breadth handle-extension support-thickness))
      (translate [0 (- (/ handle-extension 2)) (- height (/ support-thickness 2))]
                 (cube support-breadth handle-extension support-thickness)))
     cutout)))

(def thingy
  (union main-shell
         handle))

(spit "buckycup.scad"
      (write-scad thingy))
