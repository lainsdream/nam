(ns nam.anim
  (:require [reagent.core :as r]))

;; ------------------------------------------------ new-packets animation
;; Когда refresh-inbox! приносит новые пакеты, это должно быть заметно,
;; даже если сейчас открыты Rooms, а не Packets:
;;   1. если Packets сейчас свёрнуты -- сначала анимированно
;;      разворачиваем секцию (:packets-expand-progress едет 0 -> 1, тем
;;      же способом, что и :theme-progress выше -- см. animate-progress!);
;;   2. как только секция открыта (сразу, если она и так уже была
;;      открыта) -- запускаем вторую анимацию: новые строки
;;      "материализуются из ниоткуда". Это та же decay-line, что и у
;;      drop/erase, только с инвертированным прогрессом (см. reveal-line
;;      ниже) -- волна ▓▒░ идёт в обратную сторону, от пробела к тексту.
;;      Материализуются только реально новые индексы (:materializing-idxs);
;;      остальные строки рисуются как обычно.
;;
;; Обе анимации используют общий rAF-драйвер (тот же паттерн, что
;; animate-theme-progress!), но свой атом, чтобы не пересекаться с темой
;; и с long-press-прогрессом.

(def packets-expand-ms 300)
(def materialize-ms 300)

(defonce new-packets-anim-raf (atom nil))
(defonce materializing-idxs (r/atom #{}))
(defonce materialize-progress (r/atom 0))
;; :packets-expand-progress -- 0..1, 1 = обычное состояние
;; (см. секцию "new-packets animation" ниже);
;; :materializing-idxs/-progress -- какие индексы :inbox сейчас
;; "материализуются" после прихода новых пакетов и на каком
;; они этапе
(defonce expand-progress (r/atom 1))

(defn progress!
  "Гонит значение атома target-atom от текущего к 1 за duration-ms, вызывает
   on-done! по завершении. Используется и для expand, и для materialize --
   раз общий 0..1 прогресс, разный только его смысл на приёмнике."
  [duration-ms target-atom on-done!]
  (when-let [r @new-packets-anim-raf] (js/cancelAnimationFrame r) (reset! new-packets-anim-raf nil))
  (let [start-p (or @target-atom 0)
        start-t (js/performance.now)]
    (letfn
      [(tick []
        (let [t (min 1 (/ (- (js/performance.now) start-t) duration-ms))
                    p (+ start-p (* (- 1 start-p) t))]
                (reset! target-atom p)
                (if (< t 1)
                  (reset! new-packets-anim-raf (js/requestAnimationFrame tick))
                  (do (reset! new-packets-anim-raf nil) (on-done!)))))]
      (reset! new-packets-anim-raf (js/requestAnimationFrame tick)))))

(defn materialize! [n-new, total]
  (let [start-idx (max 0 (- total n-new))]
    (reset! materializing-idxs (set (range start-idx total)))
    (reset! materialize-progress 0)
    (progress! materialize-ms materialize-progress
      #(do (reset! materializing-idxs #{})
          (reset! materialize-progress 0)))))
