(ns nam.ls)

(defn- extract [k default]
  (if-let [raw (.getItem js/localStorage k)]
    (js->clj (.parse js/JSON raw) :keywordize-keys true)
    default))

(defn- set! [k v]
  (.setItem js/localStorage k (.stringify js/JSON (clj->js v))))
