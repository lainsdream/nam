(ns nam.ico)

(defn pencil-down []
  [:span {:style {:font-size "18px" :line-height 1}} "\u270E\uFE0E"])
(defn pencil []
  [:span {:style {:margin "0 0 0 6px" :font-size "18px" :line-height 1}} "\u270F\uFE0E"])
(defn black-nib []
  [:span {:style {:margin "0 0 0 6px" :font-size "18px" :line-height 1 :color "black"}} "\u2712\uFE0E"])
(defn lock []
  [:span {:style {:font-size "25px" :line-height 1}} "\uD83D\uDD10"])
(defn unlock []
  [:span {:style {:font-size "25px" :line-height 1}} "\uD83D\uDD13"])

#_(defn android []
  [:svg {:width 18 :height 18 :view-box "0 0 24 24" :fill "none" :xmlns "http://www.w3.org/2000/svg"}
   ;; обобщённый силуэт android-бота (голова + антенны + корпус), не логотип Google
   [:line {:x1 8 :y1 4.5 :x2 6.5 :y2 2.5 :stroke "currentColor" :stroke-width 1.4 :stroke-linecap "round"}]
   [:line {:x1 16 :y1 4.5 :x2 17.5 :y2 2.5 :stroke "currentColor" :stroke-width 1.4 :stroke-linecap "round"}]
   [:path {:d "M6 10.5a6 5 0 0 1 12 0z" :fill "currentColor"}]
   [:rect {:x 5.5 :y 10.5 :width 13 :height 7.5 :rx 1.5 :fill "currentColor"}]
   [:rect {:x 3.3 :y 11.5 :width 1.8 :height 5.5 :rx 0.9 :fill "currentColor"}]
   [:rect {:x 18.9 :y 11.5 :width 1.8 :height 5.5 :rx 0.9 :fill "currentColor"}]
   [:rect {:x 7.5 :y 18.5 :width 2 :height 3.3 :rx 1 :fill "currentColor"}]
   [:rect {:x 14.5 :y 18.5 :width 2 :height 3.3 :rx 1 :fill "currentColor"}]])

#_(defn seal []
  [:svg {:width 25 :height 25 :view-box "0 0 24 24" :fill "none" :xmlns "http://www.w3.org/2000/svg"}
   [:g {:transform "translate(12, 12) rotate(90) translate(-12, -12)"}  ;; Сдвиг → поворот → обратный сдвиг
    [:path {:d "M4 13a8 8 0 0 1 16 0" :stroke "currentColor" :stroke-width 1.5 :stroke-linecap "round"}]
    [:path {:d "M6.5 13a5.5 5.5 0 0 1 11 0v4" :stroke "currentColor" :stroke-width 1.5 :stroke-linecap "round"}]
    [:path {:d "M9 13a3 3 0 0 1 6 0v6" :stroke "currentColor" :stroke-width 1.5 :stroke-linecap "round"}]
    [:line {:x1 12 :y1 13 :x2 12 :y2 21 :stroke "currentColor" :stroke-width 1.5 :stroke-linecap "round"}]]])

(defn e2e []
  [:svg {:width 25 :height 25 :view-box "0 0 24 24" :fill "none" :xmlns "http://www.w3.org/2000/svg"}
   [:path {:d "M14 15a2 2 0 1 0-2 2h0m0 3a5 5 0 1 1 5-5a1.5 1.5 0 0 0 3 0a8 8 0 1 0-8 8h2M1 15c0 2.672.953 5.122 2.537 7.027M20.52 8.042A10.98 10.98 0 0 0 12 4a10.98 10.98 0 0 0-8.464 3.974m14.99-5.363A13.94 13.94 0 0 0 12 1a13.94 13.94 0 0 0-6.481 1.587"
           :stroke "currentColor" :stroke-width 2 :stroke-linecap "round"
           :fill "none"}]])

;; Форма тени -- один и тот же непрерывный p из :theme-progress двигает
;; offset круга-тени по обе стороны от 0.5: на концах (p=0 / p=1) тень
;; почти полностью перекрывает диск, оставляя тонкий полумесяц с
;; ПРОТИВОПОЛОЖНОЙ стороны (waxing справа при p=0 -- light, waning слева
;; при p=1 -- dark); к p=0.5 тень уезжает за пределы диска -- полная луна,
;; переходный момент "затмения", не отдельное состояние. Обводка на самом
;; диске (stroke) держит контур видимым даже в p=0.5, когда fill диска и
;; --bg (тень) на миг совпадают по цвету во время кроссфейда.
(defn moon [p]
    (let [cx 12 cy 12 r 8.2
        crescent-off 3    ;; толщина полумесяца на концах анимации
        full-off     20   ;; больше 2r -- тень гарантированно вне диска в p=0.5
        waxing?      (<= p 0.5)
        local-t      (if waxing? (* p 2) (* (- 1 p) 2))
        offset       (+ crescent-off (* (- full-off crescent-off) local-t))
        shadow-x     (if waxing? (- cx offset) (+ cx offset))]
    [:svg {:width 18 :height 18 :view-box "0 0 24 24" :xmlns "http://www.w3.org/2000/svg"}
     [:circle {:cx cx :cy cy :r r :fill "currentColor" :stroke "currentColor" :stroke-width 0.6}]
     [:circle {:cx shadow-x :cy cy :r r :fill "var(--bg)"}]]))

(defn bell
  ([] (bell 0))
  ([p]
   (let [angle   (* 14 (Math/sin (* 2 js/Math.PI p)))   ;; качание, p от 0 до 1
         clapper (* -0.12 angle)]
     [:svg {:width 18 :height 18 :view-box "0 0 24 24"
            :xmlns "http://www.w3.org/2000/svg"}
      [:g {:transform (str "rotate(" angle " 12 3)")}
       [:circle {:cx 12 :cy 3 :r 1.2 :fill "currentColor"}]
       [:path {:d "M12 3c-3.3 0-5.5 2.4-5.5 5.5v3.2c0 1.6-.7 2.9-1.8 4l-.7.8h16l-.7-.8c-1.1-1.1-1.8-2.4-1.8-4V8.5C17.5 5.4 15.3 3 12 3z"
               :fill "currentColor" :stroke "currentColor" :stroke-width 0.6
               :stroke-linejoin "round"}]
       [:circle {:cx (+ 12 clapper) :cy 19.5 :r 1.8 :fill "currentColor"}]]])))

(defn- sending []
  [:span.sending-icons-track
   [pencil-down] [pencil] [black-nib]
   [pencil-down] [pencil] [black-nib]])

(defn sending-viewport []
  [:span.sending-icons-viewport
   [sending]])
