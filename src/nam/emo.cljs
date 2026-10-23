(ns nam.emo
  (:require [clojure.string :as str]))

;; Официальная таблица из 333 эмодзи
;; (org/telegram/messenger/voip/EncryptionKeyEmojifier.java, DrKLO/Telegram)
(def ^:private emoji-table
  ["\uD83D\uDE09" "\uD83D\uDE0D" "\uD83D\uDE1B" "\uD83D\uDE2D" "\uD83D\uDE31" "\uD83D\uDE21" "\uD83D\uDE0E" "\uD83D\uDE34"
   "\uD83D\uDE35" "\uD83D\uDE08" "\uD83D\uDE2C" "\uD83D\uDE07" "\uD83D\uDE0F" "\uD83D\uDC6E" "\uD83D\uDC77" "\uD83D\uDC82" "\uD83D\uDC76" "\uD83D\uDC68"
   "\uD83D\uDC69" "\uD83D\uDC74" "\uD83D\uDC75" "\uD83D\uDE3B" "\uD83D\uDE3D" "\uD83D\uDE40" "\uD83D\uDC7A" "\uD83D\uDE48" "\uD83D\uDE49" "\uD83D\uDE4A"
   "\uD83D\uDC80" "\uD83D\uDC7D" "\uD83D\uDCA9" "\uD83D\uDD25" "\uD83D\uDCA5" "\uD83D\uDCA4" "\uD83D\uDC42" "\uD83D\uDC40" "\uD83D\uDC43" "\uD83D\uDC45"
   "\uD83D\uDC44" "\uD83D\uDC4D" "\uD83D\uDC4E" "\uD83D\uDC4C" "\uD83D\uDC4A" "✌" "✋" "\uD83D\uDC50" "\uD83D\uDC46" "\uD83D\uDC47" "\uD83D\uDC49"
   "\uD83D\uDC48" "\uD83D\uDE4F" "\uD83D\uDC4F" "\uD83D\uDCAA" "\uD83D\uDEB6" "\uD83C\uDFC3" "\uD83D\uDC83" "\uD83D\uDC6B" "\uD83D\uDC68\u200D\uD83D\uDC69\u200D\uD83D\uDC66" "\uD83D\uDC6C"
   "\uD83D\uDC6D" "\uD83D\uDC85" "\uD83C\uDFA9" "\uD83D\uDC51" "\uD83D\uDC52" "\uD83D\uDC5F" "\uD83D\uDC5E" "\uD83D\uDC60" "\uD83D\uDC55" "\uD83D\uDC57"
   "\uD83D\uDC56" "\uD83D\uDC59" "\uD83D\uDC5C" "\uD83D\uDC53" "\uD83C\uDF80" "\uD83D\uDC84" "\uD83D\uDC9B" "\uD83D\uDC99" "\uD83D\uDC9C" "\uD83D\uDC9A"
   "\uD83D\uDC8D" "\uD83D\uDC8E" "\uD83D\uDC36" "\uD83D\uDC3A" "\uD83D\uDC31" "\uD83D\uDC2D" "\uD83D\uDC39" "\uD83D\uDC30" "\uD83D\uDC38" "\uD83D\uDC2F"
   "\uD83D\uDC28" "\uD83D\uDC3B" "\uD83D\uDC37" "\uD83D\uDC2E" "\uD83D\uDC17" "\uD83D\uDC34" "\uD83D\uDC11" "\uD83D\uDC18" "\uD83D\uDC3C" "\uD83D\uDC27"
   "\uD83D\uDC25" "\uD83D\uDC14" "\uD83D\uDC0D" "\uD83D\uDC22" "\uD83D\uDC1B" "\uD83D\uDC1D" "\uD83D\uDC1C" "\uD83D\uDC1E" "\uD83D\uDC0C" "\uD83D\uDC19"
   "\uD83D\uDC1A" "\uD83D\uDC1F" "\uD83D\uDC2C" "\uD83D\uDC0B" "\uD83D\uDC10" "\uD83D\uDC0A" "\uD83D\uDC2B" "\uD83C\uDF40" "\uD83C\uDF39" "\uD83C\uDF3B"
   "\uD83C\uDF41" "\uD83C\uDF3E" "\uD83C\uDF44" "\uD83C\uDF35" "\uD83C\uDF34" "\uD83C\uDF33" "\uD83C\uDF1E" "\uD83C\uDF1A" "\uD83C\uDF19" "\uD83C\uDF0E"
   "\uD83C\uDF0B" "⚡" "☔" "❄" "⛄" "\uD83C\uDF00" "\uD83C\uDF08" "\uD83C\uDF0A" "\uD83C\uDF93" "\uD83C\uDF86" "\uD83C\uDF83" "\uD83D\uDC7B" "\uD83C\uDF85"
   "\uD83C\uDF84" "\uD83C\uDF81" "\uD83C\uDF88" "\uD83D\uDD2E" "\uD83C\uDFA5" "\uD83D\uDCF7" "\uD83D\uDCBF" "\uD83D\uDCBB" "☎" "\uD83D\uDCE1" "\uD83D\uDCFA"
   "\uD83D\uDCFB" "\uD83D\uDD09" "\uD83D\uDD14" "⏳" "⏰" "⌚" "\uD83D\uDD12" "\uD83D\uDD11" "\uD83D\uDD0E" "\uD83D\uDCA1" "\uD83D\uDD26" "\uD83D\uDD0C"
   "\uD83D\uDD0B" "\uD83D\uDEBF" "\uD83D\uDEBD" "\uD83D\uDD27" "\uD83D\uDD28" "\uD83D\uDEAA" "\uD83D\uDEAC" "\uD83D\uDCA3" "\uD83D\uDD2B" "\uD83D\uDD2A"
   "\uD83D\uDC8A" "\uD83D\uDC89" "\uD83D\uDCB0" "\uD83D\uDCB5" "\uD83D\uDCB3" "✉" "\uD83D\uDCEB" "\uD83D\uDCE6" "\uD83D\uDCC5" "\uD83D\uDCC1" "✂" "\uD83D\uDCCC"
   "\uD83D\uDCCE" "✒" "✏" "\uD83D\uDCD0" "\uD83D\uDCDA" "\uD83D\uDD2C" "\uD83D\uDD2D" "\uD83C\uDFA8" "\uD83C\uDFAC" "\uD83C\uDFA4" "\uD83C\uDFA7" "\uD83C\uDFB5"
   "\uD83C\uDFB9" "\uD83C\uDFBB" "\uD83C\uDFBA" "\uD83C\uDFB8" "\uD83D\uDC7E" "\uD83C\uDFAE" "\uD83C\uDCCF" "\uD83C\uDFB2" "\uD83C\uDFAF" "\uD83C\uDFC8"
   "\uD83C\uDFC0" "⚽" "⚾" "\uD83C\uDFBE" "\uD83C\uDFB1" "\uD83C\uDFC9" "\uD83C\uDFB3" "\uD83C\uDFC1" "\uD83C\uDFC7" "\uD83C\uDFC6" "\uD83C\uDFCA" "\uD83C\uDFC4"
   "☕" "\uD83C\uDF7C" "\uD83C\uDF7A" "\uD83C\uDF77" "\uD83C\uDF74" "\uD83C\uDF55" "\uD83C\uDF54" "\uD83C\uDF5F" "\uD83C\uDF57" "\uD83C\uDF71" "\uD83C\uDF5A"
   "\uD83C\uDF5C" "\uD83C\uDF61" "\uD83C\uDF73" "\uD83C\uDF5E" "\uD83C\uDF69" "\uD83C\uDF66" "\uD83C\uDF82" "\uD83C\uDF70" "\uD83C\uDF6A" "\uD83C\uDF6B"
   "\uD83C\uDF6D" "\uD83C\uDF6F" "\uD83C\uDF4E" "\uD83C\uDF4F" "\uD83C\uDF4A" "\uD83C\uDF4B" "\uD83C\uDF52" "\uD83C\uDF47" "\uD83C\uDF49" "\uD83C\uDF53"
   "\uD83C\uDF51" "\uD83C\uDF4C" "\uD83C\uDF50" "\uD83C\uDF4D" "\uD83C\uDF46" "\uD83C\uDF45" "\uD83C\uDF3D" "\uD83C\uDFE1" "\uD83C\uDFE5" "\uD83C\uDFE6"
   "⛪" "\uD83C\uDFF0" "⛺" "\uD83C\uDFED" "\uD83D\uDDFB" "\uD83D\uDDFD" "\uD83C\uDFA0" "\uD83C\uDFA1" "⛲" "\uD83C\uDFA2" "\uD83D\uDEA2" "\uD83D\uDEA4"
   "⚓" "\uD83D\uDE80" "✈" "\uD83D\uDE81" "\uD83D\uDE82" "\uD83D\uDE8B" "\uD83D\uDE8E" "\uD83D\uDE8C" "\uD83D\uDE99" "\uD83D\uDE97" "\uD83D\uDE95" "\uD83D\uDE9B"
   "\uD83D\uDEA8" "\uD83D\uDE94" "\uD83D\uDE92" "\uD83D\uDE91" "\uD83D\uDEB2" "\uD83D\uDEA0" "\uD83D\uDE9C" "\uD83D\uDEA6" "⚠" "\uD83D\uDEA7" "⛽" "\uD83C\uDFB0"
   "\uD83D\uDDFF" "\uD83C\uDFAA" "\uD83C\uDFAD" "\uD83C\uDDEF\uD83C\uDDF5" "\uD83C\uDDF0\uD83C\uDDF7" "\uD83C\uDDE9\uD83C\uDDEA" "\uD83C\uDDE8\uD83C\uDDF3"
   "\uD83C\uDDFA\uD83C\uDDF8" "\uD83C\uDDEB\uD83C\uDDF7" "\uD83C\uDDEA\uD83C\uDDF8" "\uD83C\uDDEE\uD83C\uDDF9" "\uD83C\uDDF7\uD83C\uDDFA" "\uD83C\uDDEC\uD83C\uDDE7"
   "1⃣" "2⃣" "3⃣" "4⃣" "5⃣" "6⃣" "7⃣" "8⃣" "9⃣" "0⃣" "\uD83D\uDD1F" "❗" "❓" "♥" "♦" "\uD83D\uDCAF" "\uD83D\uDD17" "\uD83D\uDD31" "\uD83D\uDD34"
   "\uD83D\uDD35" "\uD83D\uDD36" "\uD83D\uDD37"])

(defn base64url->bytes [s]
  (let [b64 (-> s (str/replace "-" "+") (str/replace "_" "/"))
        bin (js/atob b64)]
    (js/Uint8Array.from bin (fn [c] (.charCodeAt c 0)))))

(defn- concat-bytes [a b]
  (let [out (js/Uint8Array. (+ (.-length a) (.-length b)))]
    (.set out a 0)
    (.set out b (.-length a))
    out))

;; big-endian uint64 (со старшим битом очищенным) mod n,
;; посчитанный побайтово без BigInt — переполнения не будет,
;; т.к. acc всегда < n (у нас n = 333).
(defn- chunk-mod [bytes offset n]
  (loop [i 0 acc 0]
    (if (= i 8)
      acc
      (let [b (aget bytes (+ offset i))
            b (if (zero? i) (bit-and b 0x7f) b)]
        (recur (inc i) (mod (+ (* acc 256) b) n))))))

(defn- hash->emoji [hash-bytes]
  (let [n (count emoji-table)]
    (mapv #(nth emoji-table (chunk-mod hash-bytes (* % 8) n))
          (range 4))))

(defn public-key->emoji!
  "jwk — мапа/объект с полями :n и :e (base64url).
   Возвращает Promise с вектором из 4 эмодзи-фингерпринта."
  [jwk]
  (let [n-bytes (base64url->bytes (:n jwk))
        e-bytes (base64url->bytes (:e jwk))
        combined (concat-bytes n-bytes e-bytes)]
    (-> (js/crypto.subtle.digest "SHA-256" combined)
        (.then (fn [buf] (hash->emoji (js/Uint8Array. buf)))))
    ))