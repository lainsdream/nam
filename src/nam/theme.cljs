(ns nam.theme (:require [reagent.core :as r]))

;; ----------------------------------------------------------------- theme
;; Переключатель light/dark через одну иконку-луну (moon-icon ниже).
;; :theme-progress -- непрерывное 0..1, единственный источник правды и для
;; цвета луны (currentColor от --text-dim, который тут же кроссфейдится),
;; и для формы её "тени" (см. moon-icon): 0 = тонкий тёмный полумесяц на
;; светлом фоне (light), 0.5 = полная луна -- проходной момент затмения
;; посреди анимации, не отдельное состояние, 1 = тонкий светлый полумесяц
;; на тёмном фоне (dark, как сейчас по умолчанию). Клик всегда идёт через
;; полную луну независимо от направления -- p непрерывно от текущего к
;; противоположному концу, без скачков.

(defn- read []
  (case (.getItem js/localStorage "nam-theme")
    "light" :light
    "dark"  :dark
    :dark)) ; default dark, как было раньше без переключателя

;; :theme -- закоммиченное состояние (persisted в localStorage),
;; :light | :dark. Начальное значение здесь -- то, что было
;; выставлено в прошлый раз; текущая фаза луны может сразу
;; после первого рендера переписать его анимацией (см.
;; waxing-moon? и init!). :theme-progress -- 0..1, 0 =
;; light-конец, 1 = dark-конец; единственный источник правды
;; и для цветов (apply-theme-colors!), и для фазы луны
;; (moon-icon) -- см. секцию "theme" ниже.
(defonce theme (r/atom (read)))

(defn- write! [new] (.setItem js/localStorage "nam-theme" (name new)))

(defonce progress (r/atom (if (= @theme :dark) 1.0 0.0)))
(defonce anim-raf (atom nil))

(def transition-ms 1800) ;; "медленно" -- полное затмение ощущается как пауза, не мелькание

(def colors
  ;; ключ -> [light dark], те же переменные, что заданы в :root в index.html
  {:bg         ["#f0f0f2" "#0e0e12"]
   :primary    ["#e2e2e8" "#1b1b22"]
   :surface    ["#e6e6ec" "#1f1f27"]
   :text-main  ["#14141a" "#f0f0f5"]
   :text-dim   ["#5a5a66" "#a0a0ac"]
   :input-line ["#b4b4bc" "#777780"]
   :moon-key   ["#5b6ba8" "#c8d3ff"]})
  
(defn- hex->rgb [hex]
  (let [h (subs hex 1)]
    [(js/parseInt (subs h 0 2) 16)
     (js/parseInt (subs h 2 4) 16)
     (js/parseInt (subs h 4 6) 16)]))

(defn- channel->hex [n]
  (.padStart (.toString (js/Math.round n) 16) 2 "0"))

(defn- lerp-color [c1 c2 p]
  (let [[r1 g1 b1] (hex->rgb c1)
        [r2 g2 b2] (hex->rgb c2)
        mix        (fn [a b] (+ a (* (- b a) p)))]
    (str "#" (channel->hex (mix r1 r2)) (channel->hex (mix g1 g2)) (channel->hex (mix b1 b2)))))

;; p=0 -> light, p=1 -> dark; ставим как inline-стиль на :root, он
;; перекрывает значения по умолчанию из :root {} в index.html (нужны там
;; только для самой первой отрисовки до того, как этот код успел отработать)
(defn apply-colors! [p]
  (let [root (.. js/document -documentElement -style)]
    (doseq [[prop [light dark]] colors]
      (.setProperty root (str "--" (name prop)) (lerp-color light dark p)))
    (set! (.. js/document -documentElement -style -colorScheme)
          (if (>= p 0.5) "dark" "light"))))

(defn progress! [target-p]
  (when-let [r @anim-raf] (js/cancelAnimationFrame r) (reset! anim-raf nil))
  (let [start-p @progress
        start-t (js/performance.now)]
    (letfn [(tick []
              (let [t (min 1 (/ (- (js/performance.now) start-t) transition-ms))
                    p (+ start-p (* (- target-p start-p) t))]
                (reset! progress p)
                (apply-colors! p)
                (if (< t 1)
                  (reset! anim-raf (js/requestAnimationFrame tick))
                  (reset! anim-raf nil))))]
      (reset! anim-raf (js/requestAnimationFrame tick)))))

;; общая точка входа для "поставить конкретную тему с анимацией" --
;; используется и ручным toggle-theme! (всегда переключает на
;; противоположную), и авто-переходом по луне в init! (переключает именно
;; на то, что диктует текущая фаза, независимо от того, что было раньше)
(defn set-theme! [new-theme]
  (let [target-p (if (= new-theme :dark) 1.0 0.0)]
    (reset! theme new-theme)
    (write! new-theme)
    (progress! target-p)))

(defn toggle! [] (set-theme! (if (= @theme :light) :dark :light)))
