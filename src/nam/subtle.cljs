
(ns nam.subtle)

;; ------------------------------------------------------------ room-key crypto
;; Ключ комнаты -- произвольная строка (сгенерированная глифами Braille или
;; вставленная руками). Реальный AES-256-ключ всегда деривится из неё через
;; SHA-256 -- один и тот же путь что для сгенерированного, что для ручного
;; ввода, поэтому сама строка не обязана быть валидными глифами и никак не
;; валидируется при вставке (см. derive-aes-key).

(def ^:private subtle (.-subtle js/crypto))

(def braille-base 0x2800) ;; U+2800 -- начало блока Braille Patterns,
                          ;; 256 кодпоинтов подряд = 256 значений байта

(defn- byte->braille [b] (js/String.fromCodePoint (+ braille-base (bit-and b 0xff))))

(defn generate-key-glyphs
  "n-bytes случайных байт -> строка глифов Braille, один глиф на байт.
   16 байт (дефолт) = 128 бит энтропии, 16 символов на экране."
  ([] (generate-key-glyphs 16))
  ([n-bytes]
   (let [bytes (.getRandomValues js/crypto (js/Uint8Array. n-bytes))]
     (apply str (for [i (range n-bytes)] (byte->braille (aget bytes i)))))))

;; base64 <-> bytes -- нужно для упаковки iv+ciphertext в одну строку поля
;; `text`, т.к. sexp-write/sexp-read у нас только про строки, не про бинарь
(defn- bytes->base64 [bytes]
  (js/btoa (apply str (for [i (range (.-length bytes))] (js/String.fromCharCode (aget bytes i))))))

(defn- base64->bytes [b64]
  (let [bin (js/atob b64)
        n   (.-length bin)
        arr (js/Uint8Array. n)]
    (dotimes [i n] (aset arr i (.charCodeAt bin i)))
    arr))

;; Кэш CryptoKey по строке ключа -- иначе на каждое шифрование/расшифровку
;; заново гоняли бы SHA-256 + importKey, а комната может слать/читать
;; десятки пакетов подряд одним и тем же ключом
(defonce ^:private aes-key-cache (atom {}))

(defn- derive-aes-key [key-string]
  (or (get @aes-key-cache key-string)
      (let [p (-> (.digest subtle "SHA-256" (.encode (js/TextEncoder.) key-string))
                  (.then (fn [hash]
                           (.importKey subtle "raw" hash
                                       #js {:name "AES-GCM"} false #js ["encrypt" "decrypt"]))))]
        (swap! aes-key-cache assoc key-string p)
        p)))

(defn encrypt-text
  "key-string, plaintext -> Promise<строка для поля text> (base64 iv‖ciphertext)."
  [key-string plaintext]
  (-> (derive-aes-key key-string)
      (.then (fn [key]
               (let [iv (.getRandomValues js/crypto (js/Uint8Array. 12))]
                 (-> (.encrypt subtle #js {:name "AES-GCM" :iv iv}
                               key (.encode (js/TextEncoder.) plaintext))
                     (.then (fn [ct]
                              (let [ct-bytes (js/Uint8Array. ct)
                                    combined (js/Uint8Array. (+ 12 (.-length ct-bytes)))]
                                (.set combined iv 0)
                                (.set combined ct-bytes 12)
                                (bytes->base64 combined))))))))))

(defn decrypt-text
  "key-string, blob (base64 iv‖ciphertext) -> Promise<plaintext>.
   Реджектится сам, если ключ неверный -- AES-GCM детектит несовпадение
   через встроенный auth tag, отдельная проверка не нужна."
  [key-string blob]
  (-> (derive-aes-key key-string)
      (.then (fn [key]
               (let [raw (base64->bytes blob)
                     iv  (.slice raw 0 12)
                     ct  (.slice raw 12)]
                 (-> (.decrypt subtle #js {:name "AES-GCM" :iv iv} key ct)
                     (.then (fn [pt] (.decode (js/TextDecoder.) pt)))))))))

;; ------------------------------------------------------- identity keypair
;; Долгоживущая RSA-OAEP пара на устройство (не на аккаунт -- у одного
;; юзера на разных устройствах будут разные пары, это ожидаемо и не
;; является проблемой в нашей модели угроз). Приватный ключ никогда не
;; покидает устройство, публичный рассылается с каждым исходящим пакетом.
(def rsa-key-params
  #js {:name "RSA-OAEP" :modulusLength 2048
       :publicExponent (js/Uint8Array. #js [1 0 1]) :hash "SHA-256"})

(defn- generate-identity-keypair! []
  (-> (.generateKey subtle rsa-key-params true #js ["encrypt" "decrypt"])
      (.then (fn [pair]
               (-> (js/Promise.all
                     #js [(.exportKey subtle "jwk" (.-publicKey pair))
                          (.exportKey subtle "jwk" (.-privateKey pair))])
                   (.then (fn [[pub priv]]
                            {:public  (.stringify js/JSON pub)
                             :private (.stringify js/JSON priv)})))))))

(defonce ^:private rsa-public-key-cache (atom {}))
(defonce ^:private rsa-private-key-cache (atom {}))

(defn- import-rsa-public-key [jwk-string]
  (or (get @rsa-public-key-cache jwk-string)
      (let [p (.importKey subtle "jwk" (.parse js/JSON jwk-string)
                           rsa-key-params false #js ["encrypt"])]
        (swap! rsa-public-key-cache assoc jwk-string p)
        p)))

(defn- import-rsa-private-key [jwk-string]
  (or (get @rsa-private-key-cache jwk-string)
      (let [p (.importKey subtle "jwk" (.parse js/JSON jwk-string)
                           rsa-key-params false #js ["decrypt"])]
        (swap! rsa-private-key-cache assoc jwk-string p)
        p)))

(def max-keywrap-glyphs 32)

(defn wrap-glyphs
  "public-key-jwk (строка), glyphs -> Promise<base64 ciphertext>."
  [public-key-jwk glyphs]
  (-> (import-rsa-public-key public-key-jwk)
      (.then (fn [key]
               (.encrypt subtle #js {:name "RSA-OAEP"} key
                         (.encode (js/TextEncoder.) glyphs))))
      (.then (fn [ct] (bytes->base64 (js/Uint8Array. ct))))))

(defn unwrap-glyphs
  "private-key-jwk (строка), blob (base64 ciphertext) -> Promise<glyphs>."
  [private-key-jwk blob]
  (-> (import-rsa-private-key private-key-jwk)
      (.then (fn [key]
               (.decrypt subtle #js {:name "RSA-OAEP"} key (base64->bytes blob))))
      (.then (fn [pt] (.decode (js/TextDecoder.) pt)))))
