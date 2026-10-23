(ns nam.lisppad
  (:require
    [reagent.core :as r]
    [nam.toast :as toast]
    ))


(def snippet
  "(import (lispkit http) (lispkit thread future))
(eval (read (open-input-string (utf8->string (http-response-content
  (future-get (http-session-send (make-http-request \"https://lisppad.24nam.ru\" \"GET\"))))))))")

(defonce modal? (r/atom false))
(defonce copied? (r/atom false))

(defn close-modal! [] (reset! modal? false))

(defn copy-snippet! [russian?]
  (-> (.. js/navigator -clipboard (writeText snippet))
    (.then (fn [_]
      (reset! copied? true)
      (js/setTimeout #(reset! copied? false) 2000)))
    (.catch (fn [_]
      (toast/show! (if russian? "Не удалось скопировать" "Couldn't copy"))))))

(defn- modal [russian?]
  [:div.modal-overlay {:on-click close-modal!}
    [:div.modal {:on-click #(.stopPropagation %)}
    [:div.modal-title "LispPad"]
    [:pre.code-block snippet]
    [:div.btn-row
      [:button.btn.btn-accent {:on-click #(copy-snippet! russian?)}
      (cond @copied? (if russian? "скопировано ✓" "copied ✓")
            :else   (if russian? "копировать" "copy"))]
      [:button.btn {:on-click close-modal!}
      (if russian? "закрыть" "close")]]]])

(defn open-modal! [] (reset! modal? true) (reset! copied? false))
