(ns nam.toast
  (:require [reagent.core :as r]))

(defonce text (r/atom nil))

(defn show! [msg]
  (reset! text msg)
  (js/setTimeout #(reset! text nil) 3000))

(defn- racing-text [russian?]
  (if russian? "Гонка зеркал…" "Racing mirrors…"))
