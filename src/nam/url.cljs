(ns nam.url (:require
    [clojure.string :as str]
))

;; ---------------------------------------------------------------- config

;; Зеркала сервера: CF-домен и Selectel/Timeweb-сервер зеркально дополняют
;; друг друга (по VPN/без VPN/моб.сеть/wifi в РФ доступен то один, то другой).
;; Больше не пробуем их по очереди с фолбэком -- пингуем оба параллельно
;; (см. fastest-server ниже) и реальный запрос шлём сразу на того, кто
;; ответил первым.
(def servers
  ["https://del.24nam.ru"
   "https://namdel.ru"])

;; NB: параметр `time` на сервере на самом деле принимает *secret*,
;; не таймстамп -- так исторически называется в протоколе, не баг.
(defn- encode-query-val
  "Как encodeURIComponent, но не трогает ':' -- он легален в query
   по RFC 3986, а сервер ожидает его буквально в поле time."
  [s]
  (str/replace (js/encodeURIComponent s) "%3A" ":"))

(defn inbox [{:keys [nickname birthdate secret]}]
  (str "/inbox?nickname=" (encode-query-val nickname)
       "&birthdate=" (encode-query-val birthdate)
       "&time=" (encode-query-val secret)))
