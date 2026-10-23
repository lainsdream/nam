(ns nam.fx
  (:require [reagent.core :as r]
            [clojure.string :as cs]))

(defn- hash01 [n]
  (let [s (* (js/Math.sin (* n 127.1)) 43758.5453)]
    (- s (js/Math.floor s))))

(defn- vnoise [x]
  (let [i (js/Math.floor x)
        f (- x i)
        f (* f f (- 3 (* 2 f)))]
    (+ (* (hash01 i) (- 1 f)) (* (hash01 (inc i)) f))))

(defn- y-at [i x t h chaos rate]
  (let [seed (* i 31.7)
        base (- (* 2 (+ (* 0.65 (vnoise (+ (* x 0.012) (* t 0.6) seed)))
                        (* 0.35 (vnoise (+ (* x 0.05) (* t 1.7) seed 50)))))
                1)
        zone (vnoise (+ (* x 0.008) (* t 0.4) seed 200))
        lim  (- 1 chaos)
        slot (js/Math.floor (* t rate))
        kick (if (> zone lim)
               (* (- (* 2 (hash01 (+ (* x 0.37) (* slot 13.1) seed))) 1)
                  (/ (- zone lim) chaos))
               0)
        y    (+ (/ h 2) (* h 0.42 (+ (* 0.6 base) (* 0.9 kick))))]
    (max 1 (min (- h 1) y))))

(defn bolt-strip
  [{:keys [lines height steps speed chaos rate]
    :or   {lines 3 height 28 steps 90 speed 1 chaos 0.35 rate 10}}]
  (r/with-let [t   (r/atom 0)
               raf (atom nil)
               _   (letfn [(step [ts]
                             (reset! t (* speed (/ ts 1000)))
                             (reset! raf (js/requestAnimationFrame step)))]
                     (reset! raf (js/requestAnimationFrame step)))]
    (let [tt   @t
          dx   (/ 1000 steps)
          slot (js/Math.floor (* tt rate))]
      [:svg {:width "100%" :height height
             :view-box (str "0 0 1000 " height)
             :preserve-aspect-ratio "none"
             :style {:display "block"
                     :filter "drop-shadow(0 0 3px currentColor)"}}
       (for [i (range lines)]
         ^{:key i}
         [:polyline
          {:points (cs/join " "
                     (for [k (range (inc steps))
                           :let [x (if (or (zero? k) (= k steps))
                                     (* k dx)
                                     (* dx (+ k (* 0.6 (- (hash01 (+ k (* i 7))) 0.5)))))]]
                       (str x "," (y-at i x tt height chaos rate))))
           :fill "none"
           :stroke "currentColor"
           :stroke-width (+ 0.8 (* 0.3 i))
           :stroke-linejoin "miter"
           :stroke-miterlimit 4
           :vector-effect "non-scaling-stroke"
           :opacity (+ 0.45 (* 0.55 (hash01 (+ i (* slot 5.3)))))}])])
    (finally (js/cancelAnimationFrame @raf))))