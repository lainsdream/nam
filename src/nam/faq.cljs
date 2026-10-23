(ns nam.faq (:require [reagent.core :as r]))

(def text-en
  "swipe left/right - navigate Rooms/Packets

tap/click/press - 👆
tap/click/press + hold - 👆◉

👆 active `Packets` - fetches packets
👆 active `Rooms` - opens `remember room` screen

👆 a room - opens compose screen
👆 a packet - opens compose screen
-> send - drops a packet, adds to reply log

👆◉ packets group header (`── ✉ `) - opens Drop dialog
-> 👆◉ on `drop` button - drops all packets from a room

👆◉ a room - opens Drop dialog
-> 👆◉ on `drop` button - drops a room

👆◉ fingerprint - generate key
👆 unlock icon 🔓 - sign key
👆 signed key - copy key
👆 unsigned key - paste from buffer

👆 lock-with-key icon \uD83D\uDD10 - unsign key

👆◉ blue name - opens Erase dialog
-> 👆◉ `erase` button - erases the whole disk")

(def text-ru
  "свайп влево/вправо - переключение Комнаты/Пакеты

тап/клик/нажатие - 👆
тап/клик/нажатие с удержанием - 👆◉

👆 на фиолетовые `Пакеты` - забрать пакеты
👆 на фиолетовые `Комнаты` - открыть экран `запомнить комнату`

👆 по комнате - открыть диалог с комнатой
👆 по пакету - открыть диалог с комнатой
-> send - стирает пакет, записывает в лог ответов

👆◉ на строке `── ✉ ` - открыть диалог Сброса
-> 👆◉ на кнопку `сброс` - стереть все пакеты комнаты

👆◉ на комнате - открыть диалог Сброса
-> 👆◉ на кнопку `сброс` - стереть комнату

👆◉ на отпечаткe пальца - сгенерировать ключ
👆 иконки открытого замка 🔓 - подписать ключ
👆 подписанного ключа - скопировать текущий ключ
👆 неподписанного ключа - вставить из буфера

👆 иконки закрытого замка с ключиком \uD83D\uDD10 - снять подпись с ключа

👆◉ на синем имени - открыть диалог Erase
-> 👆◉ на кнопку `извлечь` - стереть весь диск")

(defonce modal? (r/atom false))

(defn- close-modal! [] (reset! modal? false))

(defn modal [russian?]
    [:div.modal-overlay {:on-click close-modal!}
     [:div.modal {:on-click #(.stopPropagation %)
     :style {:display "flex" :flex-direction "column" :max-height "80vh"}}
      [:div.modal-title "FAQ"]
      [:pre.code-block {:style {:overflow-y "auto" :flex "1 1 auto" :min-height 0}}
        (if russian? text-ru text-en)]
      [:div.btn-row
       [:button.btn {:on-click close-modal!}
        (if russian? "закрыть" "close")]]]])
