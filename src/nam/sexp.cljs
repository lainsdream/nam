(ns nam.sexp (:require
    [clojure.string :as str]
))

;; ------------------------------------------------------------- s-expr io
;; Кодек должен побайтово совпадать с python sexp_write/sexp_read
;; и scheme (write)/(read): строки в кавычках, T/NIL вместо true/nil,
;; списки в круглых скобках.
;;
;; Не трогаем ничего в этом разделе -- уже проверенно работает с сервером.

(defn write [x]
  (cond
    (nil? x)     "NIL"
    (string? x)
    (str "\"" (-> x (str/replace "\\" "\\\\") (str/replace "\"" "\\\"")) "\"")

    (boolean? x) (if x "T" "NIL")
    (number? x)  (str x)

    (or (sequential? x))
    (str "(" (str/join " " (map write x)) ")")

    :else (throw (js/Error. (str "sexp/write: unsupported " (pr-str x))))))

(defn- ws? [ch] (contains? #{\space \tab \newline \return} ch))
(defn- delim? [ch] (or (ws? ch) (= ch \() (= ch \))))

(defn read [s]
  (let [n   (count s)
        idx (atom 0)]
    (letfn [(skip-ws! [] (while (and (< @idx n) (ws? (nth s @idx))) (swap! idx inc)))
            (read-expr []
              (skip-ws!)
              (when (< @idx n)
                (let [ch (nth s @idx)]
                  (cond
                    (= ch \()
                    (do (swap! idx inc)
                        (skip-ws!)
                        (loop [items (transient [])]
                          (skip-ws!)
                          (if (and (< @idx n) (not= (nth s @idx) \)))
                            (recur (conj! items (read-expr)))
                            (do (when (< @idx n) (swap! idx inc))
                                (persistent! items)))))

                    (= ch \")
                    (do (swap! idx inc)
                        (loop [buf (transient [])]
                          (if (and (< @idx n) (not= (nth s @idx) \"))
                            (if (and (= (nth s @idx) \\) (< (inc @idx) n))
                              (do (swap! idx + 2) (recur (conj! buf (nth s (dec @idx)))))
                              (do (swap! idx inc) (recur (conj! buf (nth s (dec @idx))))))
                            (do (swap! idx inc) (str/join (persistent! buf))))))

                    :else
                    (let [start @idx]
                      (while (and (< @idx n) (not (delim? (nth s @idx)))) (swap! idx inc))
                      (let [atom-str (subs s start @idx)]
                        (cond
                          (= (str/upper-case atom-str) "NIL") []
                          (re-matches #"-?\d+" atom-str) (js/parseInt atom-str 10)
                          (re-matches #"-?\d+\.\d+" atom-str) (js/parseFloat atom-str)
                          :else atom-str)))))))]
      (read-expr))))
