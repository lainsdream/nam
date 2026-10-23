(ns nam.lp
  (:require [reagent.core :as r]))

;; --------------------------------------------------- long-press (общий)
;; Общий механизм для шапки group-header ("зажми Васю, чтобы стереть его
;; пакеты") и для кнопки drop внутри модалки подтверждения. pointer-*
;; события унифицируют мышь и тач, поэтому логика одна на оба случая.

(def long-press-ms 900) ;; деструктивное действие -- держим дольше обычного tap
;; erase-всё-и-выйти -- самое необратимое действие в приложении, поэтому
;; удержание в 2 раза дольше, чем drop пакетов одного отправителя
(def wipe-press-ms (* 2 long-press-ms))

(defonce raf (atom nil))
(defonce start-xy (atom nil))
(defonce start-ts (atom nil))
;; в какой ключ :state писать текущий 0..1 прогресс -- у drop-пакетов и у
;; erase-всё разные ключи (:drop-progress / :wipe-progress), чтобы это были
;; независимые анимации, не наступающие друг другу на пятки
(defonce progress (r/atom 0))

(def move-tolerance-px 10)

(defonce fired? (atom false))

(defn- cancel! []
  (when-let [r @raf] (js/cancelAnimationFrame r))
  (reset! raf nil)
  (reset! start-xy nil)
  (reset! start-ts nil)
  (reset! progress 0))

(defn- tick! [duration-ms on-fire!]
  (let [elapsed (- (js/performance.now) @start-ts)
        pct     (min 1 (/ elapsed duration-ms))]
    (reset! progress pct)
    (if (>= pct 1)
      (do (reset! raf nil) (reset! start-xy nil) (reset! start-ts nil)
          (reset! fired? true)
          (on-fire!))
      (reset! raf (js/requestAnimationFrame #(tick! duration-ms on-fire!))))))

;; button 0 = левая кнопка мыши; на touch-событиях (.-button) обычно nil
;; 2-арность сохраняет старое поведение (900мс, :drop-progress) для уже
;; существующих мест использования; 4-арность нужна для erase-всё (вдвое
;; дольше удержание, свой ключ прогресса)
(defn- start!
  ([e on-fire!] (start! e long-press-ms on-fire!))
  ([e duration-ms on-fire!]
   (when (or (nil? (.-button e)) (zero? (.-button e)))
     (cancel!)
     (reset! fired? false)
     (reset! start-xy [(.-clientX e) (.-clientY e)])
     (reset! start-ts (js/performance.now))
     (reset! raf (js/requestAnimationFrame #(tick! duration-ms on-fire!))))))
;; если палец/курсор увели в сторону сильнее чем на N px во время удержания
;; (скролл, дрожь), сбрасываем -- иначе случайные срабатывания при скролле

(defn- move! [e]
  (when-let [[x0 y0] @start-xy]
    (let [dx (- (.-clientX e) x0)
          dy (- (.-clientY e) y0)]
      (when (> (+ (* dx dx) (* dy dy)) (* move-tolerance-px move-tolerance-px))
        (cancel!)))))

(defn handlers
  ([on-fire!] (handlers long-press-ms on-fire!))
  ([duration-ms on-fire!]
   {:on-pointer-down   #(start! % duration-ms on-fire!)
    :on-pointer-move   move!
    :on-pointer-up     cancel!
    :on-pointer-leave  cancel!
    :on-pointer-cancel cancel!}))