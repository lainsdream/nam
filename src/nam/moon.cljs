(ns nam.moon)

(def ^:private synodic-month-days 29.53058867)
(def ^:private known-new-moon-ms (.getTime (js/Date. "2026-09-11T03:27:00Z")))

(defn moon-phase
  "Фаза луны как дробь [0, 1): 0 = новолуние, 0.5 = полнолуние."
  []
  (let [days-since (/ (- (.getTime (js/Date.)) known-new-moon-ms)
                       (* 1000 60 60 24))
        phase      (mod (/ days-since synodic-month-days) 1)]
    (if (neg? phase) (+ phase 1) phase))) ;; JS mod может дать отрицательный остаток

(defn waxing?
  "true, если луна сейчас растущая (0 -> 0.5) -- тогда тема light."
  []
  (< (moon-phase) 0.5))