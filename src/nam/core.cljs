(ns nam.core
  (:require
    [clojure.string :as str]
    [nam.anim :as anim]
    [nam.ascii :as ascii]
    [nam.call :as call]
    [nam.emo :refer [base64url->bytes public-key->emoji!]]
    [nam.faq :as faq]
    [nam.ico :as ico]
    [nam.lisppad :as lisppad]
    [nam.lp :as lp]
    [nam.ls :as ls]
    [nam.moon :as moon]
    [nam.noise :as noise]
    [nam.sexp :as sexp]
    [nam.subtle :refer
      [bytes->base64 decrypt-text encrypt-text generate-identity-keypair!
       generate-key-glyphs max-keywrap-glyphs unwrap-glyphs wrap-glyphs]]
    [nam.theme :as theme]
    [nam.toast :as toast]
    [nam.url :as url]
    [reagent.core :as r]
    [reagent.dom :as rdom]))

;; forward-декларации -- state и russian? определены ниже (в секции state),
;; а используются раньше (напр. в fastest-server/race-request), поэтому
;; shadow-cljs без этого ругается :undeclared-var warning'ами на релизной
;; сборке (сам код и так работал -- var резолвится в рантайме)
(declare state russian? decay-glyphs reveal-line reveal-compose-packet! subscribe-to-push!)

;; ------------------------------------------------------------------ net

;; резолвится либо значением fetch-fn (если успел раньше timeout-ms),
;; либо отклоняется по таймеру -- используется и для пинга (fastest-server),
;; и для самого реального запроса, чтобы зависший fetch не висел вечно.
;; В отличие от старой версии (которая просто переставала ЖДАТЬ проигравший
;; промис через Promise.race, но не останавливала сам fetch -- он продолжал
;; висеть в фоне до собственного естественного завершения/обрыва, что было
;; видно в Network-панели как зависший запрос на уже проигравший гонку
;; урл), тут fetch реально ОТМЕНЯЕТСЯ через AbortController по истечении
;; timeout-ms. Поэтому fetch-fn -- это функция от signal, а не готовый
;; промис: сам fetch должен стартовать здесь, с уже готовым signal, а не
;; быть запущен заранее вызывающим кодом.
(defn- with-timeout [fetch-fn timeout-ms]
  (let [controller (js/AbortController.)
        timer      (js/setTimeout #(.abort controller) timeout-ms)]
    (-> (fetch-fn (.-signal controller))
        (.finally #(js/clearTimeout timer)))))

;; fetch не отклоняет промис на 4xx/5xx сам по себе -- проверяем :ok руками,
;; иначе вызывающий код (http-get/http-post) примет ошибочный ответ за успех
(defn- fetch-sexp [url signal]
  (-> (js/fetch url #js {:cache "no-store" :signal signal})
      (.then (fn [resp]
               (if (.-ok resp)
                 (.text resp)
                 (js/Promise.reject (js/Error. (str "HTTP " (.-status resp)))))))
      (.then sexp/read)))

(defn- fetch-sexp-post [url body signal]
  (-> (js/fetch url #js {:method  "POST"
                         :headers #js {"Content-Type" "text/plain; charset=utf-8"}
                         :body    (sexp/write body)
                         :signal  signal})
      (.then (fn [resp]
               (if (.-ok resp)
                 (.text resp)
                 (js/Promise.reject (js/Error. (str "HTTP " (.-status resp)))))))
      (.then sexp/read)))

;; настоящая гонка: пингуем все server-urls ПАРАЛЛЕЛЬНО, берём того, кто
;; первым ответил (сам факт валидного ответа -- см. ping-url), не
;; дожидаясь второго -- даже если тот завис или ещё не уложился в свой
;; собственный таймаут. js/Promise.any игнорирует отказавшие промисы и
;; резолвится первым успешным; если отказали все -- падает с AggregateError.
(def ping-timeout-ms 5000)

;; для пинга важен не просто факт какого-то ответа, а именно РЕАЛЬНЫЙ
;; ответ настоящего сервера: "/" отдаёт 200 с телом "healthy". Раньше тут
;; засчитывался любой резолвнувшийся fetch (в т.ч. 404) как "сервер жив" --
;; так что DPI/прокси/VPN, подсовывающий свой быстрый фейковый ответ на
;; этот домен (не пропуская реальный трафик до сервера), выигрывал гонку
;; пинга вместо настоящего живого зеркала. Поэтому теперь проверяем и
;; :ok, и тело -- если это не тот самый "healthy", считаем пинг
;; неудавшимся, как будто сервер вообще не ответил.
(defn- ping-url [url]
  (-> (with-timeout
        (fn [signal] (js/fetch url #js {:cache "no-store" :signal signal}))
        ping-timeout-ms)
      (.then (fn [resp]
               (if (.-ok resp)
                 (.text resp)
                 (js/Promise.reject (js/Error. (str "HTTP " (.-status resp)))))))
      (.then (fn [body]
               (if (= (str/trim body) "healthy")
                 url
                 (js/Promise.reject (js/Error. (str "фейковый пинг, тело: " (pr-str body)))))))))

;; если оба пинга отвалились -- не показываем пользователю ошибку, а тихо
;; перезапускаем гонку. Реальные обрывы сети и так дают естественную паузу
;; между попытками (fetch висит до ping-timeout-ms прежде чем отвалиться),
;; так что сервер этим не "долбится" -- разве что при мгновенных отказах
;; (например connection refused без реального таймаута) итерации могут
;; идти чаще; если это станет проблемой, тут же можно добавить пол-
;; секундную паузу перед повтором.
;;
;; round считает попытки: 0 -- первая, обычная, без индикации (это
;; ожидаемая задержка первого пинга, показывать тост тут смысла нет).
;; Начиная со второго раунда (round > 0, то есть первая попытка уже
;; провалилась) держим persistent-тост "гонка зеркал…" -- он не
;; автоскрывается по таймеру, как обычный show-toast!, а живёт ровно
;; пока идёт повтор гонки, и снимается сам при удачном пинге.

(defn- fastest-server
  ([] (fastest-server 0))
  ([round]
   (-> (js/Promise.any (into-array (map ping-url url/servers)))
       (.then (fn [url]
                (js/console.debug "ping race: winner" url)
                url))
       (.catch (fn [_]
                 (js/console.debug "ping race: раунд" round "не удался, повторяем")
                 (when (zero? round)
                   (reset! toast/text (toast/racing-text (russian?))))
                 (fastest-server (inc round)))))))

;; таймаут на сам реальный запрос. 30с, а не 5с (как у пинга) -- пинг
;; почти пустой (просто "жив ли сервер"), а тут может передаваться целый
;; инбокс, плюс сама обработка на сервере, так что запросу честно нужно
;; больше времени, особенно на медленной сети. Раньше здесь стояло 15с,
;; и при СТАБИЛЬНО медленной (а не просто изредка глючащей) сети запрос
;; каждый раз упирался в один и тот же потолок и ретраился по кругу
;; бесконечно -- увеличенный ретраем шанс не давал ничего, потому что
;; сеть не флуктуирует, а равномерно медленная. Поэтому таймаут реального
;; запроса больше не растим по раундам и не ретраим при его провале (см.
;; race-request) -- только пинг всё ещё ретраится (это оправдано: один из
;; двух урлов может быть "недоступен для реальных запросов", даже если
;; отвечает на пинг).
(def request-timeout-ms 30000)

;; Ретраим только пинг (fastest-server сам крутит свой цикл, пока не
;; найдёт живое зеркало) -- а вот таймаут САМОГО реального запроса больше
;; НЕ ретраим: если honest 30с не хватило, это либо реальная проблема
;; (сервер/сеть), либо тот самый "второй урл живой для пинга, но мёртвый
;; для реальных запросов" случай -- в обоих случаях бесконечный цикл
;; здесь бесполезен, лучше сразу показать ошибку вызывающему коду
;; (refresh-inbox!/send-from-compose! и так покажут её тостом).
(defn- race-request [path make-fetch round]
  (-> (fastest-server round)
      (.then (fn [url]
               (with-timeout
                 (fn [signal] (make-fetch (str url path) signal))
                 request-timeout-ms)))
      (.then (fn [result]
               ;; успех -- если тост "гонка зеркал…" всё ещё висит после
               ;; ретраев пинга, снимаем его именно тут (после реального
               ;; успеха), а не сразу по пингу -- иначе он гаснет раньше
               ;; времени, пока сам запрос ещё идёт
               (when (= @toast/text (toast/racing-text (russian?)))
                 (reset! toast/text nil))
               result))))

(defn- http-post [path body]
  (race-request path (fn [url signal] (fetch-sexp-post url body signal)) 0))

(defn- http-get [path]
  (race-request path (fn [url signal] (fetch-sexp url signal)) 0))

(defn- own-public-key []
  (get-in @state [:me :identity-keypair :public]))

(defn send-packet! [me to to-bd text signed keywrap]
  (http-post "/send" [(:nickname me) (:birthdate me) (:secret me) to to-bd text signed keywrap (own-public-key)]))

(defn- packet-signature [pkt]
  [(nth pkt 0 nil) (nth pkt 1 nil) (nth pkt 2 nil) (nth pkt 3 nil) (nth pkt 4 nil)])

(defn- dedupe-pending [inbox pending]
  (let [existing (set (map packet-signature inbox))]
    (remove #(contains? existing (packet-signature %)) pending)))

(defn ack-packet! [me index]
  (http-post "/ack" [(:nickname me) (:birthdate me) index]))

(defn- ack-sequentially! [me indices]
  (reduce
    (fn [chain i]
      (-> chain
          (.then (fn [] (ack-packet! me i)))
          (.catch (fn [e]
                    (js/console.warn "ack failed for index" i e)))))
    (js/Promise.resolve)
    indices))

(defn- empty-response? [x] (or (nil? x) (and (sequential? x) (empty? x))))
(defn- sexp->bool [x] (= x "T"))
(defn- sexp->maybe-str [x] (if (string? x) x nil))

;; ack идёт в обратном порядке (как и в python/scheme клиентах) --
;; так индексы уже полученных пакетов не съезжают между ack-вызовами.
(defn fetch-inbox! [me on-new!]
  (-> (http-get (url/inbox me))
      (.then (fn [result]
               (let [raw     (if (empty-response? result) [] result)
                     pending (mapv (fn [[from time text signed keywrap public-key]]
                                     [from time (sexp->maybe-str text) (sexp->bool signed)
                                      (sexp->maybe-str keywrap) (sexp->maybe-str public-key) nil])
                                   raw)]
                 (when (seq pending)
                   (on-new! pending)
                   (ack-sequentially! me (range (dec (count pending)) -1 -1)))
                 pending)))))

;; ------------------------------------------------------------ formatting
;; Зеркалит data/Models.kt из android-клиента.

(defn- pad [n width]
  (.padStart (str n) width "0"))

(defn make-birthdate
  "(\"2001\" \"3\" \"9\") -> \"2001-03-09\", как data/Models.kt makeBirthdate."
  [year month day]
  (str (pad year 4) "-" (pad month 2) "-" (pad day 2)))

(defn format-birthdate-pipes
  "\"YYYY-MM-DD\" -> \"(D-M-YYYY)\", без ведущих нулей, как в python/kotlin клиентах."
  [bd]
  (let [[y m d] (str/split bd #"-")]
    (str "(" (js/parseInt d 10) "-" (js/parseInt m 10) "-" (js/parseInt y 10) ")")))

(defn mask-secret [secret] (apply str (repeat (count secret) "•")))

(defn truncate [s max-len]
  (if (<= (count s) max-len) s (str (subs s 0 max-len) "…")))

(defn- digits-only [s] (str/replace s #"[^0-9]" ""))

;; --------------------------------------------------------------- state

(defn- lang-get []
  (case (.getItem js/localStorage "nam-language")
    "ru" :ru
    "en" :en
    :en)) ; default ENGLISH, как Store.loadLanguage

(defn- lang-set! [lang] (.setItem js/localStorage "nam-language" (name lang)))

(defn- fresh-setup-form []
  {:index 0 :nickname "" :year "" :month "" :day "" :secret "" :text "" :error nil})

(defn- fresh-wizard-form []
  {:index 0 :nickname "" :year "" :month "" :day "" :text ""})

;; ------------------------------------------------------------ moon phase
;; При заходе тема сначала берётся из localStorage (то, что было выставлено
;; в прошлый раз -- вручную или той же луной), без анимации, чтобы не было
;; вспышки дефолтной темы из index.html. А сразу после первого рендера
;; (см. init!) проверяем текущую фазу луны, и если она требует другой
;; темы -- проигрываем ту же анимацию перехода, что и при ручном
;; toggle-theme!, так пользователь видит, как его прошлая настройка
;; переписывается статусом луны, а не просто видит её уже готовой.
;;
;; Формула без сети: берём известное новолуние как точку отсчёта, считаем
;; сколько дней прошло, делим на длину синодического месяца. Погрешность
;; растёт со временем от референса, но на горизонте десятков лет остаётся
;; в пределах пары часов -- более чем достаточно для UI-переключателя.

(defn- parse-me [raw-json]
  (when raw-json
    (let [obj   (.parse js/JSON raw-json)
          rk    (aget obj "room-keys")]
      (-> (js->clj obj :keywordize-keys true) ; nickname/birthdate/secret/contacts -- ок, keywordize нужен
          (assoc :room-keys
                 (if rk
                   (into {} (for [k (js/Object.keys rk)] [k (aget rk k)])) ; ключи остаются строками -- ни разу не проходят через keywordize
                   {}))))))

(defonce state
  (r/atom (let [me (parse-me (.getItem js/localStorage "nam-config"))]
            {:language      (lang-get)
             :me            me                  ; {:nickname :birthdate :secret :contacts}
             :inbox         (ls/extract "nam-inbox" []) ; [[from time text] ...]
             ;; локальный лог "их пакет -> наш ответ" по нику. Плоский
             ;; вектор записей {:nickname :them :us}, НЕ map с никнеймами
             ;; как ключами -- тот же класс бага, что был с room-keys и
             ;; keywordize-keys. Новые записи -- в начало (cons).
             :replylog      (ls/extract "nam-replylog" [])
             :screen        (if me :main :language) ; :language | :setup | :main | :compose | :wizard
             :target        nil                  ; [:contact idx] | [:reply idx]
             :compose-info  nil                  ; {:nickname :detail}
             :draft         ""
             :expanded      :packets              ; :rooms | :packets
             ;; индекс пакета в :inbox, для которого сейчас в compose играет
             ;; анимация "шум рассеивается" после успешной подписи ключа
             ;; (см. reveal-compose-packet! ниже); nil -- анимации нет
             :compose-reveal-idx      nil
             :compose-reveal-progress 1
             :secret-visible? false
             :setup-form    (fresh-setup-form)
             :wizard-form   (fresh-wizard-form)
             :sending?      false
             :key-drafts       {} ; {nickname -> glyph-string}, только в памяти, не на диске
              :key-pending?     {} ; {nickname -> true}, пока идёт фоновая отправка keywrap
              :key-holding?     false
             })))

(defn- persist-me! [] (ls/set! "nam-config" (:me @state)))
(defn- persist-inbox! [] (ls/set! "nam-inbox" (:inbox @state)))
(defn- persist-replylog! [] (ls/set! "nam-replylog" (:replylog @state)))

;; identity-keypair живёт внутри :me (persist-me! и так сохраняет весь :me
;; целиком), рядом с room-keys. Генерируется один раз на устройство --
;; если её ещё нет (новый юзер или обновление со старой версии без неё).
(defn- ensure-identity-keypair! []
  (when (and (:me @state) (not (get-in @state [:me :identity-keypair])))
    (-> (generate-identity-keypair!)
        (.then (fn [kp]
                 (swap! state assoc-in [:me :identity-keypair] kp)
                 (persist-me!))))))

;; пишем только при реальном ответе на чужой пакет (kind = :reply), не
;; при инициировании переписки через контакт -- это лог ответов, а не
;; лог всей переписки
(defn- append-replylog! [nickname them us]
  (swap! state update :replylog #(vec (cons {:nickname nickname :them them :us us} %)))
  (persist-replylog!))

(defn- russian? [] (= (:language @state) :ru))

(defn- find-contact [me nickname]
  (first (filter #(= (first %) nickname) (:contacts me))))

(defn- packet-signed?     [pkt] (nth pkt 3 false))
(defn- packet-raw-text    [pkt] (nth pkt 2 nil))
(defn- packet-keywrap     [pkt] (nth pkt 4 nil))
(defn- packet-public-key  [pkt] (nth pkt 5 nil))
(defn- packet-decrypted   [pkt] (nth pkt 6 nil))
(defn- packet-draft       [pkt] (nth pkt 7 nil))

;---
(defn- room-key-for [nickname]
  (get-in @state [:me :room-keys nickname]))

;; последний известный публичный ключ этого ника -- берём из его самого
;; свежего пакета в :inbox (порядок хронологический, новые в конце,
;; см. refresh-inbox!/dedupe-pending). nil, если от этого ника ещё вообще
;; не было ни одного пакета -- тогда отправить keywrap невозможно.
(defn- last-known-public-key [nickname]
  (->> (:inbox @state)
       (filter #(= (first %) nickname))
       (keep packet-public-key)
       last))

(defn- can-send-keywrap? [nickname]
  (boolean (last-known-public-key nickname)))

;; тихая фоновая отправка глифов, зашифрованных публичным ключом
;; получателя. Промис резолвится/реджектится -- вызывающий код (do-sign!)
;; сам решает, что делать по результату (применить ключ локально / откатить
;; seal). Текст пакета -- всегда nil, вся полезная нагрузка в keywrap.
(defn- send-keywrap! [nickname glyphs]
  (let [me  (:me @state)
        pub (last-known-public-key nickname)
        bd  (second (find-contact me nickname))]
    (if-not pub
      (js/Promise.reject (js/Error. "no known public key for room"))
      (-> (wrap-glyphs pub glyphs)
          (.then (fn [wrapped]
                   (send-packet! me nickname bd nil true wrapped)))))))

(defn- packet-display-text [pkt]
  (cond
    (packet-decrypted pkt)     (packet-decrypted pkt)
    (packet-keywrap pkt)       (noise/line (hash (packet-keywrap pkt)) (min 30 (count (packet-keywrap pkt))))
    (not (packet-signed? pkt)) (packet-raw-text pkt)
    :else                      (noise/line (hash (packet-raw-text pkt)) (min 30 (count (packet-raw-text pkt))))))

(defn- key-ever-generated? [nickname]
  (or (contains? (:key-drafts @state) nickname)
      (boolean (room-key-for nickname))))

(defn- key-signed-visual? [nickname]
  (let [draft   (get-in @state [:key-drafts nickname])
        disk    (room-key-for nickname)
        pending (get-in @state [:key-pending? nickname])]
    (boolean (or pending (and draft disk (= draft disk))))))

(defn- key-current-glyphs [nickname]
  (or (get-in @state [:key-drafts nickname]) (room-key-for nickname) ""))

(defn- paste-into-key! [nickname]
  (when-not (key-signed-visual? nickname)
    (-> (.readText (.-clipboard js/navigator))
        (.then (fn [txt]
                 (when (seq txt)
                   (swap! state assoc-in [:key-drafts nickname]
                          (subs txt 0 (min max-keywrap-glyphs (count txt)))))))
        (.catch (fn [_] nil)))))

(defn- copy-key! [nickname]
  (when (key-signed-visual? nickname)
    (-> (.writeText (.-clipboard js/navigator) (get-in @state [:key-drafts nickname]))
        (.then (fn [_] (toast/show! (if (russian?) "скопировано" "copied"))))
        (.catch (fn [_] nil)))))

;;;
;; расшифровывает пакет idx текущим ключом key; проверяет, что ciphertext
;; в :inbox по этому индексу всё ещё тот же (на случай если пока промис
;; летел, пакет удалили/индексы сдвинулись) -- если нет, тихо не трогает
(defn- decrypt-and-store! [idx ciphertext key]
  (-> (decrypt-text key ciphertext)
      (.then (fn [plain]
               (let [new-state (swap! state update :inbox
                                  (fn [ib]
                                    (if (and (< idx (count ib)) (= (nth (nth ib idx) 2 nil) ciphertext))
                                      (assoc-in ib [idx 6] plain)
                                      ib)))
                     applied?  (= (packet-decrypted (nth (:inbox new-state) idx nil)) plain)]
                 (persist-inbox!)
                 (let [{:keys [screen target]} new-state
                       [kind ref] target]
                   (when (and applied? (= screen :compose) (= kind :reply) (= ref idx))
                     (reveal-compose-packet! idx))))))
      (.catch (fn [_] nil)))) ;; неверный ключ -- остаётся шумом, это ок

;; проходит по ВСЕЙ истории отправителя (не только по новым пакетам) --
;; вызывается на каждый sign!, поэтому расшифрованное только растёт
(defn- backfill-decrypt! [nickname]
  (when-let [key (room-key-for nickname)]
    (doseq [[i pkt] (map-indexed vector (:inbox @state))]
      (when (and (= (first pkt) nickname)
                 (packet-signed? pkt)
                 (nil? (packet-decrypted pkt)))
        (decrypt-and-store! i (packet-raw-text pkt) key)))))

(defn sign-room-key! [nickname key-string]
  (swap! state update-in [:me :room-keys] assoc nickname key-string)
  (swap! state assoc-in [:key-drafts nickname] key-string)
  (persist-me!)
  (backfill-decrypt! nickname))
;;;

;; keywrap-пакет несёт RSA-зашифрованные глифы вместо обычного AES-текста
;; (text = nil, keywrap = base64 blob). Расшифровываем своим приватным
;; ключом и сразу применяем как room-key для отправителя -- без
;; подтверждения со стороны получателя (см. обсуждение Q3/Q4).
(defn- unwrap-and-sign! [idx pkt]
  (let [from (first pkt)
        blob (packet-keywrap pkt)
        priv (get-in @state [:me :identity-keypair :private])]
    (when (and blob priv)
      (-> (unwrap-glyphs priv blob)
          (.then (fn [glyphs]
                   (when (seq glyphs)
                     (swap! state update :inbox
                            (fn [ib]
                              (if (and (< idx (count ib)) (= (packet-keywrap (nth ib idx)) blob))
                                (assoc-in ib [idx 6] glyphs)
                                ib)))
                     (persist-inbox!)
                     (sign-room-key! from glyphs))))
          (.catch (fn [_] nil))))))
 ;; не наш keywrap / битые данные -- тихо игнорируем

;; тихая фоновая отправка при sign: seal сразу становится серым (визуально
;; "подписано"), но реальные глифы применяются в :room-keys только после
;; успешной отправки get-keywrap собеседнику. Неудача -- откатываем seal
;; обратно на красный (do-unsign! без побочных сетевых эффектов).
(defn- do-sign! [nickname]
  (let [draft (get-in @state [:key-drafts nickname])]
    (when (seq draft)
      (if (can-send-keywrap? nickname)
        (do
          (swap! state assoc-in [:key-pending? nickname] true)
          (-> (send-keywrap! nickname draft)
              (.then (fn [_]
                       (swap! state update :key-pending? dissoc nickname)
                       (sign-room-key! nickname draft)))
              (.catch (fn [e]
                        (swap! state update :key-pending? dissoc nickname)
                        (swap! state update :key-drafts dissoc nickname)
                        (toast/show! (str (if (russian?) "Ошибка отправки ключа: " "Key send error: ")
                                           (.-message e)))))))
        ;; нет public key контакта -- старый ручной путь, мгновенно и
        ;; без сети, глифы можно скопировать и переслать самому
        (sign-room-key! nickname draft)))))

;; unsign мгновенный (без css-transition) -- временно глушим transition
;; на обёртке через reflow-трюк, чтобы цвета не "доезжали" плавно
(defn- do-unsign! [nickname]
  (when-let [el (js/document.querySelector (str "[data-key-room=\"" nickname "\"]"))]
    (.add (.-classList el) "no-key-transition")
    (js/requestAnimationFrame
      #(js/requestAnimationFrame (fn [] (.remove (.-classList el) "no-key-transition")))))
  (swap! state update :key-drafts dissoc nickname))

;; ------------------------------------------------------------ room-key widget

#_(defn- toggle-key-expanded! [nickname]
  (swap! state update-in [:key-expanded? nickname] not)
  ;; сидируем draft с диска один раз, если комната уже подписана, а в
  ;; этой сессии draft к ней ещё не трогали -- иначе на свежей загрузке
  ;; страницы уже подписанная комната отрисуется как "не подписано"
  (when (and (not (contains? (:key-drafts @state) nickname)) (room-key-for nickname))
    (swap! state assoc-in [:key-drafts nickname] (room-key-for nickname))))

;; --- удержание ключика: единый raf-луп, никаких отдельных таймеров.
;; elapsed < key-tap-ms         -> короткий тап, просто toggle expand
;; elapsed >= key-tap-ms        -> начинаем реролл глифов + auto-expand
;; отпустили ДО key-commit-ms и это первая-в-жизни генерация -> откат
;; во всех остальных случаях -- фиксируем то, что успело нарисоваться
(def ^:private key-tap-ms 180)
(def ^:private key-commit-ms 500)

(defonce ^:private hold-raf (atom nil))
(defonce ^:private hold-press-ts (atom 0))
(defonce ^:private hold-last-tick (atom 0))
(defonce ^:private hold-was-generated? (atom false))

(defn- stop-key-raf! []
  (when @hold-raf (js/cancelAnimationFrame @hold-raf) (reset! hold-raf nil)))

(defn- start-key-hold! [nickname]
  (reset! hold-press-ts (js/Date.now))
  (reset! hold-last-tick 0)
  (reset! hold-was-generated? (key-ever-generated? nickname))
  (swap! state assoc :key-holding? true)
  (letfn [(tick [ts]
            (when (:key-holding? @state)
              (let [elapsed (- (js/Date.now) @hold-press-ts)]
                (when (and (>= elapsed key-tap-ms) (> (- ts @hold-last-tick) 90))
                  (reset! hold-last-tick ts)
                  (swap! state assoc-in [:key-drafts nickname] (generate-key-glyphs))))
              (reset! hold-raf (js/requestAnimationFrame tick))))]
    (reset! hold-raf (js/requestAnimationFrame tick))))

(defn- end-key-hold! [nickname]
  (swap! state assoc :key-holding? false)
  (stop-key-raf!)
  (let [elapsed (- (js/Date.now) @hold-press-ts)]
    (when (and (not @hold-was-generated?) (< elapsed key-commit-ms))
      (swap! state update :key-drafts dissoc nickname)))) ; откат, только для самой первой генерации

;; ------------------------------------------------------------ render

(defn- room-key-widget [nickname]
  (let [signed?  (key-signed-visual? nickname)
        pending? (get-in @state [:key-pending? nickname])
        rotated? (key-ever-generated? nickname)
        glyphs   (key-current-glyphs nickname)]
    [:div.key-widget {:data-key-room nickname}
        [:span.key-icon-wrap
          {:class (when signed? "is-moon")
            :on-pointer-down   (when-not (or signed? pending?) #(start-key-hold! nickname))
            :on-pointer-up     (when-not (or signed? pending?) #(end-key-hold! nickname))
            :on-pointer-leave  (when-not (or signed? pending?) #(end-key-hold! nickname))
            :on-pointer-cancel (when-not (or signed? pending?) #(end-key-hold! nickname))}
          [ico/e2e]]
      (when rotated?
        [:span.key-glyphs {:class (if signed? "is-moon" "is-gray")
                           :on-click (when-not pending? #(if signed? (copy-key! nickname) (paste-into-key! nickname)))}
        glyphs])

      (when rotated?
        [:span.key-seal {:on-click (when-not pending? #(if signed? (do-unsign! nickname) (do-sign! nickname)))}
        (if (or signed? pending?) [ico/lock] [ico/unlock])])
     ]))

(defn- announce-new-packets! [n-new]
  (when (pos? n-new)
    (let [total     (count (:inbox @state))
          start-idx (max 0 (- total n-new))
          new-idxs  (set (range start-idx total))]
      (if (not= (:expanded @state) :packets)
        (do
          (reset! anim/expand-progress 0)
          (reset! anim/materializing-idxs new-idxs) ;; помечаем сразу
          (swap! state assoc :expanded :packets)
          (anim/progress! anim/packets-expand-ms anim/expand-progress
            #(anim/materialize! n-new total)))
        (anim/materialize! n-new total)))))

;; -------------------------------------------------------------- events

(defn select-language! [lang]
  (swap! state assoc :language lang)
  (lang-set! lang)
  (swap! state assoc :screen :setup :setup-form (fresh-setup-form))
  (r/flush))

(defn setup! [nickname birthdate secret]
  (swap! state assoc :me {:nickname nickname :birthdate birthdate :secret secret :contacts []})
  (persist-me!)
  (swap! state assoc :screen :main)
  (ensure-identity-keypair!))

(defn add-contact! [nickname birthdate]
  (swap! state update-in [:me :contacts] conj [nickname birthdate])
  (persist-me!))

(defn refresh-inbox! []
  (let [me (:me @state)]
    (when me
      (let [count-before (count (:inbox @state))]
        (-> (fetch-inbox! me
              (fn [pending]
                (let [deduped   (dedupe-pending (:inbox @state) pending)
                      start-idx (count (:inbox @state))]
                  (when (seq deduped)
                    (swap! state update :inbox into deduped)
                    (persist-inbox!)
                    (doseq [[i pkt] (map-indexed (fn [i p] [(+ start-idx i) p]) deduped)]
                      (let [from (first pkt)
                            key  (room-key-for from)]
                        (cond
                          (packet-keywrap pkt)        (unwrap-and-sign! i pkt)
                          (and (packet-signed? pkt) key) (decrypt-and-store! i (packet-raw-text pkt) key)
                          :else nil)))))))
            (.then (fn [_]
                     (let [n-new (- (count (:inbox @state)) count-before)]
                       (when (pos? n-new) (announce-new-packets! n-new)))))
            (.catch (fn [e]
                      (toast/show! (str (if (russian?) "Ошибка загрузки: " "Inbox fetch error: ")
                                         (.-message e))))))))))

(defn toggle-secret! [] (swap! state update :secret-visible? not))

(defn open-compose! [nickname detail kind ref]
  (when (and (room-key-for nickname)
             (not (contains? (:key-drafts @state) nickname)))
    (swap! state assoc-in [:key-drafts nickname] (room-key-for nickname)))
  (swap! state assoc
         :screen :compose
         :compose-info {:nickname nickname :detail detail}
         :target [kind ref]
         :draft (or (when (= kind :reply) (packet-draft (nth (:inbox @state) ref nil))) ""))
  (r/flush))

(defn- save-reply-draft! []
  (let [{:keys [target draft]} @state
        [kind ref] target]
    (when (and (= kind :reply) ref (< ref (count (:inbox @state))) (seq draft))
      (swap! state assoc-in [:inbox ref 7] draft)
      (persist-inbox!))))

(defn back-to-main! []
  (save-reply-draft!)
  (swap! state assoc :screen :main :target nil :compose-info nil :draft "")
  (refresh-inbox!))

(defn send-from-compose! [text kind ref on-done]
  (if (str/blank? text)
    (on-done)
    (let [me      (:me @state)
          to-nick (if (= kind :contact)
                    (first (nth (:contacts me) ref))
                    (first (nth (:inbox @state) ref)))
          key     (room-key-for to-nick)
          signed? (boolean key)]
      (swap! state assoc :sending? true)
      (-> (if key (encrypt-text key text) (js/Promise.resolve text))
          (.then (fn [payload]
                   (if (= kind :contact)
                     (let [[nick bd] (nth (:contacts me) ref)]
                       (send-packet! me nick bd payload signed? nil))
                     (let [[from]    (nth (:inbox @state) ref)
                           them-text (packet-display-text (nth (:inbox @state) ref))
                           contact   (find-contact me from)]
                       (if contact
                         (-> (send-packet! me (first contact) (second contact) payload signed? nil)
                             (.then (fn [_]
                                      (swap! state update :inbox
                                             #(vec (concat (subvec % 0 ref) (subvec % (inc ref)))))
                                      (persist-inbox!)
                                      (append-replylog! from them-text text))))
                         (js/Promise.reject
                           (js/Error. (if (russian?) "Отправитель не найден в контактах" "Sender not in contacts"))))))))
          (.then (fn [_] (swap! state assoc :sending? false) (on-done)))
          (.catch (fn [e]
                    (swap! state assoc :sending? false)
                    (toast/show! (str (if (russian?) "Ошибка отправки: " "Send error: ") (.-message e)))
                    (on-done)))))))

;; ----------------------------------------------------------- setup form

(defn- setup-step-value [f]
  (case (:index f) 0 (:nickname f) 1 (:year f) 2 (:month f) 3 (:day f) (:secret f)))

(defn- setup-assoc-field [f idx v]
  (case idx 0 (assoc f :nickname v) 1 (assoc f :year v) 2 (assoc f :month v) 3 (assoc f :day v) (assoc f :secret v)))

(defn setup-continue! []
  (let [f        (:setup-form @state)
        idx      (:index f)
        text     (:text f)
        numeric? (contains? #{1 2 3} idx)
        last?    (= idx 4)]
    (cond
      (str/blank? text)
      (swap! state assoc-in [:setup-form :error]
             (if (russian?) "Поле не может быть пустым" "This field cannot be empty"))

      (and numeric? (not (re-matches #"\d+" text)))
      (swap! state assoc-in [:setup-form :error]
             (if (russian?) "Введите число" "Enter a number"))

      last?
      (let [{:keys [nickname year month day]} f]
        (setup! nickname (make-birthdate year month day) (str/trim text)))

      :else
      (let [f2 (-> f
                   (setup-assoc-field idx (if (zero? idx) (str/trim text) text))
                   (update :index inc)
                   (assoc :error nil))
            f3 (assoc f2 :text (setup-step-value f2))]
        (swap! state assoc :setup-form f3)
        (r/flush)))))

(defn setup-back! []
  (let [f (:setup-form @state)]
    (if (zero? (:index f))
      ;; аналог onQuit() в android-клиенте (там завершает activity) --
      ;; в вебе некуда "выходить", возвращаемся к выбору языка.
      (swap! state assoc :screen :language)
      (let [f2 (-> f (update :index dec) (assoc :error nil))
            f3 (assoc f2 :text (setup-step-value f2))]
        (swap! state assoc :setup-form f3)
        (r/flush)))))

;; ---------------------------------------------------------- wizard form
;; (визард добавления контакта, "запомнить комнату")

(defn- wizard-step-value [f]
  (case (:index f) 0 (:nickname f) 1 (:year f) 2 (:month f) (:day f)))

(defn- wizard-assoc-field [f idx v]
  (case idx 0 (assoc f :nickname v) 1 (assoc f :year v) 2 (assoc f :month v) (assoc f :day v)))

(defn start-wizard! []
  (swap! state assoc :wizard-form (fresh-wizard-form) :screen :wizard)
  (r/flush))

(defn wizard-back! []
  (let [f (:wizard-form @state)]
    (if (zero? (:index f))
      (swap! state assoc :screen :main)
      (let [f2 (-> f (update :index dec))
            f3 (assoc f2 :text (wizard-step-value f2))]
        (swap! state assoc :wizard-form f3)
        (r/flush)))))

(defn- finish-wizard! [nickname bd]
  (let [me (:me @state)]
    (if (find-contact me nickname)
      (toast/show! (if (russian?)
                     (str "Контакт «" nickname "» уже существует")
                     (str "Contact '" nickname "' already exists")))
      (add-contact! nickname bd))
    (swap! state assoc :wizard-form (fresh-wizard-form) :screen :main)))

(defn wizard-continue! []
  (let [f    (:wizard-form @state)
        idx  (:index f)
        text (:text f)]
    (cond
      (and (zero? idx) (str/blank? text))
      (toast/show! (if (russian?) "Поле не может быть пустым" "This field cannot be empty"))

      (and (pos? idx) (or (str/blank? text) (not (re-matches #"\d+" text))))
      (toast/show! (if (russian?) "Введите число" "Enter a number"))

      (= idx 3)
      (finish-wizard! (:nickname f) (make-birthdate (:year f) (:month f) text))

      :else
      (let [f2 (-> f (wizard-assoc-field idx text) (update :index inc))
            f3 (assoc f2 :text (wizard-step-value f2))]
        (swap! state assoc :wizard-form f3)
        (r/flush)))))

;; ------------------------------------------------------------- ui bits

(defn- forward-button [{:keys [is-last? label on-click class]}]
  [:button.btn {:class class :on-click on-click}
   (if is-last? label (str label " →"))])

;; авто-рост textarea: сбрасываем высоту, затем ставим по scrollHeight,
;; чтобы поле росло только вместе с переносами строк, а не заранее
(defn- autosize-textarea! [el]
  (when el
    (set! (.. el -style -height) "auto")
    (set! (.. el -style -height) (str (.-scrollHeight el) "px"))))

;; именованная (не анонимная) функция -> React не считает ref "новым"
;; на каждый ре-рендер, фокус ставится один раз при маунте компонента,
;; а не при каждом изменении текста
(defn- compose-textarea-ref [el]
  (when el
    (.focus el)
    (let [len (.. el -value -length)]
      (.setSelectionRange el len len))
    (autosize-textarea! el)))

;; для однострочных полей setup/wizard -- та же идея, что и выше, без
;; авто-роста. Заменяет :auto-focus true: HTML-атрибут autofocus не
;; гарантированно триггерит фокус/клавиатуру на элементе, смонтированном
;; уже после начальной загрузки страницы (а у нас именно так на каждом
;; шаге формы) -- явный .focus() в ref куда надёжнее.
(defn- focus-input-ref [el]
  (when el (.focus el)))

;; ------------------------------------------------------------ screens

(defn language-screen []
  [:div.screen.language-screen
   [:div.lang-option {:on-click #(select-language! :ru)} "русский"]
   [:div.lang-option {:on-click #(select-language! :en)} "english"]])

(defn- latin-lower-only [s]
  (-> s
      str/lower-case
      (str/replace #"[^a-z0-9]:" "")))

(defn setup-screen []
  (let [f            (:setup-form @state)
        russian?     (russian?)
        idx          (:index f)
        numeric?     (contains? #{1 2 3} idx)
        last?        (= idx 4)
        placeholders (if russian? ["имя" "год" "месяц" "день" "секрет"]
                                   ["name" "year" "month" "day" "secret"])
        forward      (cond (and last? russian?) "конец"
                            last?                "end"
                            russian?             "дальше"
                            :else                "forward")
        backward     (cond (and (zero? idx) russian?) "выйти"
                            (zero? idx)                "quit"
                            russian?                   "назад"
                            :else                       "back")]
    [:div.screen.setup-screen
     [:div.title.title-red (if russian? "ВХОД" "ENTER")]
     [:input.underline-field.own-id-color
      {:key            idx ; ремоунт поля на каждом шаге -> фокус (через :ref) срабатывает заново
       :type           "text"
       :inputMode      (when numeric? "numeric")
       :auto-capitalize "off" ; не капитализировать первую букву имени/секрета на мобильной клавиатуре
       :ref            focus-input-ref
       :value          (:text f)
       :placeholder    (nth placeholders idx)
       :on-change #(swap! state assoc-in [:setup-form :text]
                    (let [v (.. % -target -value)]
                      (if numeric?
                        (digits-only v)
                        (latin-lower-only v))))
       :on-key-down    #(when (= (.-key %) "Enter") (setup-continue!))}]
     (when (:error f) [:div.error-text (:error f)])
     [:div.btn-row
      [forward-button {:is-last? last? :label forward :on-click setup-continue! :class "btn-forward"}]
      [:button.btn {:on-click setup-back!} backward]]]))

(defn wizard-screen []
  (let [f            (:wizard-form @state)
        russian?     (russian?)
        idx          (:index f)
        numeric?     (pos? idx)
        last?        (= idx 3)
        placeholders (if russian? ["имя" "год" "месяц" "день"]
                                   ["name" "year" "month" "day"])
        forward      (cond (and last? russian?) "конец"
                            last?                "end"
                            russian?             "дальше"
                            :else                "forward")
        backward     (cond (and (zero? idx) russian?) "выйти"
                            (zero? idx)                "quit"
                            russian?                   "назад"
                            :else                       "back")]
    [:div.screen.wizard-screen
     [:div.title.title-red (if russian? "запомнить комнату" "remember room")]
     [:input.underline-field
      {:key            idx
       :type           "text"
       :inputMode      (when numeric? "numeric")
       :auto-capitalize "off" ; не капитализировать первую букву имени на мобильной клавиатуре
       :ref            focus-input-ref
       :value          (:text f)
       :placeholder    (nth placeholders idx)
       :on-change      #(swap! state assoc-in [:wizard-form :text]
                                (let [v (.. % -target -value)] (if numeric? (digits-only v) v)))
       :on-key-down    #(when (= (.-key %) "Enter") (wizard-continue!))}]
     [:div.btn-row
      [forward-button {:is-last? last? :label forward :on-click wizard-continue! :class "btn-forward"}]
      [:button.btn {:on-click wizard-back!} backward]]]))

;; ------------------------------------------------------- clear packets
;; "Своей" истории не существует -- на диске лежат только входящие пакеты.
;; Зажатие group-header конкретного отправителя открывает модалку.
;;
;; Одно значение :drop-progress (0..1, растёт пока держишь кнопку drop)
;; управляет сразу тремя вещами синхронно:
;;   1. прозрачностью самой модалки -- чем дольше держишь, тем прозрачнее
;;      она становится, и сквозь неё виден фон;
;;   2. текстовым распадом элементов модалки (заголовок, drop, назад) --
;;      тот же мотив плотности ▓▒░, что и в deco-строке "▓▒░ N a M ░▒▓";
;;   3. точно таким же распадом реальных строк пакетов этого отправителя
;;      на фоне (в main-screen) -- то есть то, что видно сквозь
;;      прозрачнеющую модалку, и есть настоящие данные, которые вот-вот
;;      исчезнут, а не отдельная бутафория внутри модалки.
;; Отпустил раньше времени -- lp-cancel! мгновенно откатывает всё обратно.
;; Дошёл до конца -- модалка закрывается (она и так уже прозрачна), фон
;; ещё немного держит полностью распавшийся текст, и только затем пакеты
;; реально стираются из :inbox.

(def decay-glyphs ["▓" "▒" "░"])
(def decay-window 0.28) ;; доля прогресса, за которую символ проходит ▓→▒→░→пробел

;; детерминированный (не случайный на каждый кадр) порог распада символа --
;; иначе вместо волны получится мерцание. Разный seed на строку даёт
;; разный рисунок стагера.
(defn- decay-threshold [seed i]
  (/ (mod (+ (* (mod seed 9973) 131) (* i 977)) 1000) 1000))

(defn- decay-char [ch idx seed progress]
  (if (= ch \space)
    " "
    (let [th    (decay-threshold seed idx)
          local (/ (- progress th) decay-window)]
      (cond
        (<= local 0) (str ch)
        (>= local 1) " "
        :else (nth decay-glyphs (min 2 (int (* local 3))))))))

;; progress масштабируется так, чтобы к progress=1 ВСЕ символы (включая
;; те, у кого порог близок к 1) успели дойти до пробела
(defn- decay-line [s seed progress]
  (let [eff (min 1 (/ progress (- 1 decay-window)))]
    (apply str (map-indexed (fn [i ch] (decay-char ch i seed eff)) s))))

;; обратная анимация к decay-line -- строка "материализуется из ниоткуда"
;; (см. announce-new-packets! выше): progress 0 = ещё полностью распавшаяся
;; (невидима), 1 = обычный читаемый текст. Тот же волновой рисунок ▓▒░,
;; просто прогон времени в обратную сторону -- поэтому просто инвертируем
;; progress и переиспользуем decay-line как есть.
(defn- reveal-line [s seed progress]
  (decay-line s seed (- 1 progress)))

;; ---------------------------------------------------- compose reveal anim
;; Тот же приём, что у materialize-new-rows! выше, но для одного конкретного
;; пакета: если мы сидим в compose на ещё-шумном пакете и в этот момент
;; подписываем ключ отправителя (sign-room-key! -> backfill-decrypt! ->
;; decrypt-and-store!), не подменяем текст мгновенно, а проигрываем ту же
;; волну ▓▒░ через reveal-line -- шум "рассеивается" в прочитанный текст.
(def compose-reveal-ms 500)
(defonce compose-reveal-raf (atom nil))

(defn- reveal-compose-packet! [idx]
  (when-let [r @compose-reveal-raf] (js/cancelAnimationFrame r) (reset! compose-reveal-raf nil))
  (swap! state assoc :compose-reveal-idx idx :compose-reveal-progress 0)
  (let [start-t (js/performance.now)]
    (letfn [(tick []
              (let [t (min 1 (/ (- (js/performance.now) start-t) compose-reveal-ms))]
                (swap! state assoc :compose-reveal-progress t)
                (if (< t 1)
                  (reset! compose-reveal-raf (js/requestAnimationFrame tick))
                  (do (reset! compose-reveal-raf nil)
                      (swap! state assoc :compose-reveal-idx nil :compose-reveal-progress 1)))))]
      (reset! compose-reveal-raf (js/requestAnimationFrame tick)))))

(defn- clear-target-count [from]
  (count (filter #(= (first %) from) (:inbox @state))))

(defn open-clear-modal!
  ([from] (open-clear-modal! from (clear-target-count from)))
  ([from n]
    (reset! lp/progress 0)
    (swap! state assoc :clear-target from :clear-target-n n :clear-modal? true )))

(defn close-clear-modal! []
  (lp/cancel!)
    (reset! lp/progress 0)
  (swap! state assoc :clear-modal? false :clear-target nil))

(defn- remove-contact! [nick]
  (swap! state update-in [:me :contacts]
         (fn [contacts] (vec (remove #(= (first %) nick) contacts))))
  (swap! state update-in [:me :room-keys] dissoc nick)
  (swap! state update :replylog
         (fn [log] (vec (remove #(= (:nickname %) nick) log))))
  (persist-me!)
  (persist-replylog!)
  (close-clear-modal!))

;; после того, как long-press на кнопке drop дошёл до 1: закрываем модалку
;; (она и так уже почти прозрачна), даём фону ещё чуть-чуть подержать
;; полностью распавшийся текст, и только потом реально удаляем из :inbox
(defn- finalize-clear-packets! []
  (let [from (:clear-target @state)]
    (swap! state update :inbox #(vec (remove (fn [pkt] (= (first pkt) from)) %)))
    (persist-inbox!))
  (reset! lp/progress 0)
  (swap! state assoc :clear-target nil)
  (toast/show! (if (russian?) "пакеты удалены" "packets deleted")))

(defn- do-clear-packets! []
  (swap! state assoc :clear-modal? false) ;; :drop-progress остаётся 1, :clear-target остаётся -- фон ещё распавшийся
  (js/setTimeout finalize-clear-packets! 300))

;; --------------------------------------------------------- erase all
;; Долгое нажатие на свою шапку (ник + дата, own-id) -- по тому же
;; механизму, что и group-header отправителя -- открывает модалку с erase
;; (аналог drop, но держать в 2 раза дольше -- wipe-press-ms) и back.
;;
;; :wipe-progress двигает те же три вещи, что :drop-progress у clear-modal:
;; прозрачность оверлея, распад текста внутри модалки, и распад ВСЕГО
;; фона main-screen (кроме строки-деко "▓▒░ N a M ░▒▓", которая остаётся
;; нетронутой как единственный якорь на экране). Поскольку удержание вдвое
;; длиннее, а прогресс идёт от 0 до 1 за то же кол-во кадров относительно
;; своей длительности, визуально распад фона получается вдвое медленнее,
;; чем при обычном drop одного отправителя -- отдельно замедлять decay-line
;; не нужно.
;;
;; :wiping? живёт отдельно от :wipe-modal?, аналогично :clear-target
;; отдельно от :clear-modal? -- когда удержание erase доходит до конца,
;; модалка сразу закрывается (она и так уже прозрачна), а фон должен ещё
;; на мгновение подержать полностью распавшийся вид, прежде чем реально
;; всё стирается и происходит переход на экран выбора языка.

(defn open-wipe-modal! []
  (reset! lp/progress 0)
  (swap! state assoc :wipe-modal? true :wiping? true))

(defn close-wipe-modal! []
  (lp/cancel!)
  (reset! lp/progress 0)
  (swap! state assoc :wipe-modal? false :wiping? false :secret-visible? false))

(defn- clear-all-caches! []
  (when-let [c (.-caches js/window)]
    (-> (.keys c)
        (.then (fn [ks] (doseq [k ks] (.delete c k))))
        (.catch (fn [_])))))

;; тот самый "начальный экран" -- ровно то же самое, с чего приложение
;; стартует у нового пользователя без nam-config в localStorage
(defn- finalize-wipe! []
  (.clear js/localStorage)
  (clear-all-caches!)
  (reset! anim/materializing-idxs #{})
  (reset! anim/materialize-progress 0)
  (reset! anim/expand-progress 1)
  (reset! lisppad/modal? false)
  (reset! lisppad/copied? false)
  (reset! toast/text nil)
  (reset! faq/modal? false)
  (reset! lp/progress 0)
  (reset! state {:language        :en
                 :me              nil
                 :inbox           []
                 :screen          :language
                 :target          nil
                 :compose-info    nil
                 :draft           ""
                 :expanded        :packets
                 :secret-visible? false
                 :setup-form      (fresh-setup-form)
                 :wizard-form     (fresh-wizard-form)
                 :sending?        false
                 :clear-modal?    false
                 :clear-target    nil
                 :wipe-modal?     false
                 :wiping?         false
                 }))

(defn- do-wipe! []
  (swap! state assoc :wipe-modal? false) ;; :wiping?/:wipe-progress остаются -- фон ещё полностью распавшийся
  (js/setTimeout finalize-wipe! 300))

(defn- group-inbox
  "Группирует индексы inbox по отправителю, сохраняя порядок первого
   появления -- как LinkedHashMap в MainScreen.kt."
  [inbox]
  (let [order (atom []) idxs (atom {})]
    (doseq [[i pkt] (map-indexed vector inbox)]
      (let [from (first pkt)]
        (when-not (contains? @idxs from) (swap! order conj from))
        (swap! idxs update from (fnil conj []) i)))
    (mapv (fn [from] [from (get @idxs from)]) @order)))

(def section-move-ms 320)
(def section-reveal-ms 300)

(defn- section-align [section]
  (if (= section :rooms) "right" "left"))

(defn- section-slide-class [section]
  (if (= section :rooms) "section-slide-out-right" "section-slide-out-left"))

(defn- body-position [section]
  (if (= section :packets) 0 -50))

(defn- start-section-switch! [target]
  (when-not (:section-anim @state)
    (swap! state assoc :section-anim {:phase :moving :target target})
    (js/setTimeout
      (fn []
        (swap! state assoc :section-anim {:phase :revealing :target target})
        (js/setTimeout
          (fn []
            (swap! state assoc :expanded target :section-anim nil))
          section-reveal-ms))
      section-move-ms)))

(defonce swipe-start-x (atom nil))
(defonce swipe-start-y (atom nil))
(def swipe-threshold-px 50)

(defn- swipe-end! [e active]
  (when-let [x0 @swipe-start-x]
    (let [dx (- (.-clientX e) x0)
          dy (- (.-clientY e) (or @swipe-start-y 0))]
      (when (and (> (js/Math.abs dx) swipe-threshold-px)
                 (> (js/Math.abs dx) (js/Math.abs dy)))
        (cond
          (and (neg? dx) (= active :packets)) (start-section-switch! :rooms)
          (and (pos? dx) (= active :rooms))   (start-section-switch! :packets)
          :else nil))))
  (reset! swipe-start-x nil)
  (reset! swipe-start-y nil))

(defn- swipe-start! [e]
  (reset! swipe-start-x (.-clientX e))
  (reset! swipe-start-y (.-clientY e)))

(defn- section-header-text [label expanded-glyph collapsed-glyph expanded? left right]
  (let [g (if expanded? expanded-glyph collapsed-glyph)]
    (str g left " " label " " right g)))

(defn main-screen []
  (let [{:keys [me inbox expanded clear-target 
                wiping? section-anim]} @state
        progress @lp/progress
        russian?          (russian?)
        senders           (set (map first inbox))
        rooms-expanded?   (= expanded :rooms)
        materialize-progress @anim/materialize-progress
        materializing-idxs (or @anim/materializing-idxs #{})
        expand-progress (if (nil? @anim/expand-progress) 1 @anim/expand-progress)
        rooms-label       (if russian? "КОМНАТЫ" "ROOMS")
        packets-label     (if russian? "ПАКЕТЫ" "PACKETS")
        nobody-yet        (if russian? "· пока никого ·" "· nobody yet ·")
        all-quiet         (if russian? "· тишина ·" "· all quiet ·")
        dw                (fn [s seed] (if wiping? (decay-line s seed progress) s))
        active            (if rooms-expanded? :rooms :packets)
        inactive          (if rooms-expanded? :packets :rooms)
        anim-target       (:target section-anim)
        body-section      (or anim-target active) ;; куда должна смотреть рельса прямо сейчас
        glyph-active?     (fn [section] (if anim-target (= section anim-target) (= section active)))
        header-label (fn [section]
               (let [active? (glyph-active? section)]
                 (case section
                   :rooms   (dw (section-header-text rooms-label "✦" "✧" active? "┄☆\uFE0E┄☉\uFE0E┄☾\uFE0E" "☽┄☉\uFE0E\uFE0E┄") (if active? 8 6))
                    :packets (dw (section-header-text packets-label "◆" "◇" active? "•⁘⁙⟦" "⟧·•∙⁘⁙·") (if active? 8 6)))))]
    [:div.screen.main-screen
     [:div {:style {:display "flex" :gap "1ch"}}
      [:div.deco "▓▒░"]
      [:div.row-line {:class (when wiping? "is-decaying")}
        [:span.own-id.own-header
          (merge (lp/handlers open-wipe-modal!) {:style {:font-size 18}})
          (dw (:nickname me) 2)]]
      [:div.deco "░▒▓"]
     ]

     [:div.section-headers
      (cond
        (nil? section-anim)
        [:<>
         [:div.section-header.slot-top.unselect
          {:class    (when wiping? "is-decaying")
           :on-click (when-not wiping? #(start-section-switch! inactive))
           :style    {:text-align (section-align inactive)}}
          (header-label inactive)]
         [:div.section-header.expanded.slot-bottom.unselect
          {:class    (when wiping? "is-decaying")
           :on-click (when-not wiping? #(if rooms-expanded? (start-wizard!) (refresh-inbox!)))
           :style    {:text-align (section-align active)}}
          (header-label active)]]

        (= (:phase section-anim) :moving)
        [:<>
         [:div.section-header.expanded.slot-bottom
          {:class (section-slide-class active)
           :style {:text-align (section-align active) :color "var(--text-dim)"}}
          (header-label active)]
         [:div.section-header.slot-top.section-move-down
          {:style {:text-align (section-align inactive) :color "var(--accent)"}}
          (header-label inactive)]]

        (= (:phase section-anim) :revealing)
        [:<>
         [:div.section-header.expanded.slot-bottom
          {:style {:text-align (section-align anim-target)}}
          (header-label anim-target)]
         [:div.section-header.slot-top.section-fade-in
          {:style {:text-align (section-align active)}}
          (header-label active)]])]

     ;; --- контент секций: обе панели всегда в DOM, свайп через translateX рельсы ---
     [:div.section-body-viewport
      {:on-pointer-down (when-not (:section-anim @state) #(swipe-start! %))
        :on-pointer-up   (when-not (:section-anim @state) #(swipe-end! % active))}
      [:div.section-body-track
        {:style {:transform (str "translateX(" (body-position body-section) "%)")}}
        [:div.section-body.styled-scroll.scroll-left {:class (when wiping? "is-decaying")
                     :style {:opacity          expand-progress
                             :transform        (str "scaleY(" expand-progress ")")
                             :transform-origin "top"}}
        (if (empty? inbox)
          [:div.dim-msg (dw all-quiet 9)]
          (for [[from idxs] (group-inbox inbox)]
            (let [sender-decaying?      (= from clear-target)
                  decaying?             (or wiping? sender-decaying?)
                  header-text           (str "⌁⌁ ✉\uFE0E " from " ⌁⌁⌁⌁⌁⌁⌁⌁⌁⌁")
                  header-materializing? (and (seq materializing-idxs)
                                              (every? materializing-idxs idxs))]
              ^{:key from}
              [:div
               [:div.group-header
                (merge
                  (when-not wiping? (lp/handlers #(open-clear-modal! from)))
                  {:class (cond decaying? "is-decaying" header-materializing? "is-materializing")
                    :style {:text-align "left"}})
                (cond
                  decaying?             (decay-line header-text (hash from) progress)
                  header-materializing? (reveal-line header-text (hash from) materialize-progress)
                  :else                 header-text)]
               (for [i idxs]
                (let [row-text               (str "  " (truncate (packet-display-text (nth inbox i)) 30))
                  row-materializing?     (contains? materializing-idxs i)
                  hidden-during-expand?  (and row-materializing? (< expand-progress 1))]
                    (when-not hidden-during-expand?
                      ^{:key i}
                      [:div.row {:class    (cond decaying? "is-decaying"
                                                  row-materializing? "is-materializing")
                                :on-click (when-not (or decaying? row-materializing?)
                                            #(open-compose! from (packet-display-text (nth inbox i)) :reply i))
                                :style {:text-align "left"}}
                      (cond
                        decaying?          (decay-line row-text (+ (hash from) i 1) progress)
                        row-materializing? (reveal-line row-text (+ (hash from) i 1) materialize-progress)
                        :else              row-text)])) 
                 )])))]

       [:div.section-body.styled-scroll {:class (when wiping? "is-decaying")}
        (if (empty? (:contacts me))
          [:div.dim-msg {:style {:text-align "right"}} (dw nobody-yet 7)]
          (for [[i [nick bd]] (map-indexed vector (:contacts me))]
            ^{:key i}
            [:div.row.room-row.unselect
              (merge
                (when-not wiping? (lp/handlers #(open-clear-modal! nick nil)))
                {:on-click #(if @lp/fired? (reset! lp/fired? false) (open-compose! nick nil :contact i))
                  :on-context-menu #(.preventDefault %)
                  :style {:text-align "right"}})
              (dw (str (if (contains? senders nick) "✦" "✧") "  " nick "  " (format-birthdate-pipes bd))
                  (+ 100 i))]))]]]]))

(defn parse-backticks [text]
  (let [parts (str/split text #"`" -1)]
    (into [:<>]
      (map-indexed
        (fn [idx part]
          (if (odd? idx)
            ^{:key idx} [:code.copy-field
                         {:on-click #(js/navigator.clipboard.writeText part)}
                         part]
            ^{:key idx} part))
        parts))))

(defn replylog-view []
  (let [container-ref (atom nil)
        idle-timer    (atom nil)]
    (letfn [(animate-scroll-to-top! [^js el]
              (set! (.. el -style -scrollBehavior) "smooth")
              (set! (.-scrollTop el) 0))
            (schedule-idle-scroll! [el]
              (when @idle-timer (js/clearTimeout @idle-timer))
              (reset! idle-timer
                (js/setTimeout #(animate-scroll-to-top! el) 3000)))
            (on-scroll [e]
              (schedule-idle-scroll! (.-target e)))]
      (r/create-class
        {:component-did-mount
         (fn [] (when-let [el @container-ref] (schedule-idle-scroll! el)))
         :component-will-unmount
         (fn []
           (when @idle-timer (js/clearTimeout @idle-timer)))
         :reagent-render
         (fn [log]
           [:div.replylog.styled-scroll
            {:ref       #(reset! container-ref %)
             :on-scroll on-scroll}
            (doall
              (map-indexed
                (fn [idx {:keys [them us]}]
                  ^{:key idx}
                  [:<>
                  [:div.row-line {:style {:text-align "left"}} (parse-backticks us)]
                  [:div.row-line {:style {:text-align "right"}} (parse-backticks them)]])
                log))])}))))

(defn compose-screen []
  (let [{:keys [compose-info draft sending? target inbox replylog
                compose-reveal-idx compose-reveal-progress me]} @state
        russian?   (russian?)
        [kind ref] target
        bd  (if
          (= kind :contact)
          (second (nth (:contacts me) ref))
          (second (find-contact me (first (nth (:inbox @state) ref))))
        )
        nickname   (:nickname compose-info)
        ;; для :reply берём текст живьём из :inbox (а не замороженный на
        ;; момент open-compose! :detail) -- иначе успевшая расшифроваться
        ;; в фоне (backfill-decrypt!) строка так и останется шумом на
        ;; экране до выхода из compose и повторного входа
        live-text  (cond
                     (not= kind :reply) (:detail compose-info)
                     (and (>= ref 0) (< ref (count inbox))) (packet-display-text (nth inbox ref))
                     :else (:detail compose-info))
        revealing? (and (= kind :reply) (= compose-reveal-idx ref))
        log        (filter #(= (:nickname %) nickname) replylog)]
    [:div.screen.compose-screen
     [:div.row-line [room-key-widget nickname]]
     [:div.row-line {:style {:display "flex" :justify-content "space-between"} }
      [:span] [call/call-button (:me @state) nickname bd russian?]]
     [:div.row-line
      {:style {:text-align "right"}}
      [:span
        (r/with-let [emoji (r/atom nil)]
          (when-let [key-str (last-known-public-key nickname)]
            (-> 
              (.parse js/JSON key-str)
              (js->clj :keywordize-keys true)
              (public-key->emoji!)
              (.then (fn [result] (reset! emoji result)))
              (.catch (fn [e] (js/console.error "Error:" e)))))
          [:span (clojure.string/join " " @emoji)])

     [:span " < "]
     [:span.title-red {:style {:font-size 23}} nickname]]]
     ;; текст/шум чужого пакета -- вправо, тем же краем, что и key-widget
     ;; в хедере выше
     [:div.row-line {:style {:text-align "right"}}
      (when (seq live-text)
        [:span.ascii {:class (when revealing? "is-materializing")}
         (if revealing?
           (reveal-line live-text (+ (hash nickname) ref 1) compose-reveal-progress)
           (parse-backticks live-text))])]
     [:textarea.underline-field.autosize
      {:rows      1
        :value     draft
        :ref       compose-textarea-ref
        :style     {:resize "none" :overflow "hidden"}
        :on-change #(let [el (.. % -target)]
                      (swap! state assoc :draft (.-value el))
                      (autosize-textarea! el))
        :on-key-down
          #(when (and (= (.-key %) "Enter")
                      (not (.-shiftKey %))
                      (not sending?)
                      (seq (clojure.string/trim draft)))
            (.preventDefault %)
            (send-from-compose! draft kind ref back-to-main!))}]
     [:div.btn-row
      (if sending?
        [:button.btn.btn-sending {:disabled true} [ico/sending-viewport]]
        (list
          ^{:key "send"}
           [:button.btn-accent.btn
            {:on-click #(send-from-compose! draft kind ref back-to-main!)}
            [ico/pencil-down]
            [ico/pencil]
            [ico/black-nib]]
          ^{:key "back"}
          [:button.btn {:on-click back-to-main!} (if russian? "назад" "back")]))]
     ;; сам replylog: новые записи уже лежат в начале вектора (append-replylog!
     ;; делает cons), поэтому просто разворачиваем каждую запись в порядке
     ;; хранения -- us (наш ответ, слева) сразу над them (их пакет, справа)
     (when (seq log)
      [replylog-view])
    ]))

(defn- top-icons []
  (let [wiping? (:wiping? @state) p @theme/progress]
    [:div.top-icons {:class (when wiping? "is-decaying")}
     [:button.icon-btn {:title "FAQ" :disabled wiping?  :on-click (when-not wiping? #(reset! faq/modal? true))} "؟"]
     [:button.icon-btn {:title "LispPad" :disabled wiping? :on-click (when-not wiping? lisppad/open-modal!)} "λ"]
     [:button.icon-btn
      {:title "Theme" :disabled wiping?
        :on-click #(do
          (subscribe-to-push! (:nickname (:me @state)) (:birthdate (:me @state)))
          (theme/toggle!))}
      [ico/moon p]]]))

;; Подтверждение удаления пакетов конкретного отправителя. Модалка -- те
;; же 2 строки, что и раньше (заголовок + btn-row), без списка пакетов:
;; сам список -- это фон, который становится виден по мере того, как
;; модалка прозрачнеет. Один :drop-progress двигает три вещи разом:
;; прозрачность оверлея, распад текста внутри модалки (заголовок, drop,
;; назад) и распад настоящих строк пакетов на фоне (см. main-screen).
(defn- clear-packets-modal []
  (let [russian?   (russian?)
        from       (:clear-target @state)
        n          (:clear-target-n @state)
        progress   @lp/progress
        header     (if (nil? n) from (str from " > " (if russian? "пакеты" "packets") " (" n ")"))
        back-lbl   (if russian? "назад" "back")
        drop-lbl   (if russian? "сброс" "drop")
        on-drop-fn (if (nil? n) #(remove-contact! from) do-clear-packets!)]
    [:div.modal-overlay.clear-overlay
     {:style    {"--drop-progress" progress}
      :on-click close-clear-modal!}
     [:div.modal {:on-click #(.stopPropagation %)}
      [:div.clear-modal-header
       (if-not (pos? progress) header
         (let [decayed (decay-line header (hash from) progress)]
           (if-not (str/blank? decayed) decayed (first decay-glyphs))))]
      [:div.btn-row
       [:button.btn.btn-drop
        (merge {:style {"--drop-progress" progress} :class (when (pos? progress) "is-charging")}
         (lp/handlers on-drop-fn))
        (if (pos? progress) (decay-line drop-lbl 777 progress) drop-lbl)]
       [:button.btn {:on-click close-clear-modal!}
        (if (pos? progress) (decay-line back-lbl 555 progress) back-lbl)]]]]))

;; Подтверждение полного стирания -- та же механика, что у clear-packets-
;; -modal (drop-progress -> прозрачность/распад), но:
;;   * над кнопками -- строка "ник (d-m-YYYY)", тем же цветом/классом
;;     own-id, что и на экране за модалкой (не заголовок с количеством,
;;     как у per-sender clear);
;;   * "erase" вместо "drop" -- держать нужно wipe-press-ms (в 2 раза
;;     дольше long-press-ms);
;;   * распад фона (см. main-screen) идёт от :wipe-progress, а не
;;     :drop-progress -- и, т.к. сам wipe-progress растёт вдвое медленнее
;;     реального времени, визуально весь экран расползается вдвое дольше,
;;     чем пакеты одного отправителя при обычном drop.
(defn- wipe-packets-modal []
  (let [{:keys [me secret-visible? wiping?]} @state
        russian?  (russian?)
        progress  @lp/progress
        id-line   (str (:nickname me) "  " (format-birthdate-pipes (:birthdate me)))
        back-lbl  (if russian? "назад" "back")
        dw        (fn [s seed] (if wiping? (decay-line s seed progress) s))
        ]
    [:div.modal-overlay.clear-overlay
     {:style    {"--drop-progress" progress}
      :on-click close-wipe-modal!}
     [:div.modal {:on-click #(.stopPropagation %)}
      
      [:div {:style {:display "flex" :justify-content "space-between"}}
        [:div.clear-modal-header.own-id
          (if (pos? progress)
            (let [decayed (decay-line id-line (hash id-line) progress)]
              (if (str/blank? decayed) (first decay-glyphs) decayed))
            id-line)
        ]
        [:span.clear-modal-header
          {:on-click toggle-secret!
          :style    {:cursor "pointer" :user-select "none"}}
          (dw (if secret-visible? (:secret me) (mask-secret (:secret me))) 4)]]
      [:div.btn-row
       [:button.btn.btn-drop
        (merge (lp/handlers lp/wipe-press-ms do-wipe!)
          {:style {"--drop-progress" progress}
            :class (when (pos? progress) "is-charging")})
        (let [erase-lbl (if russian? "извлечение" "erase")]
          (if (pos? progress) (decay-line erase-lbl 777 progress) erase-lbl))]
       [:button.btn {:on-click close-wipe-modal!}
        (if (pos? progress) (decay-line back-lbl 555 progress) back-lbl)]]]]))

(defn root []
  (let [{:keys [screen clear-modal? wipe-modal?]} @state]
    [:div.app
     [ascii/bg]
     (when-not (= screen :compose) [top-icons])
     (case screen
       :language [language-screen]
       :setup    [setup-screen]
       :main     [main-screen]
       :compose  [compose-screen]
       :wizard   [wizard-screen]
       [main-screen])
     (when @faq/modal? [faq/modal (russian?)])
     (when @lisppad/modal? [lisppad/modal (russian?)])
     (when clear-modal? [clear-packets-modal])
     (when wipe-modal? [wipe-packets-modal])
     (when @toast/text [:div.toast @toast/text])]))

(defn- register-service-worker! []
  (when (.-serviceWorker js/navigator)
    (-> (js/fetch "/sw-version.json" #js {:cache "no-store"})
        (.then (fn [resp] (.json resp)))
        (.then (fn [manifest]
                 (.register (.-serviceWorker js/navigator) (.-file manifest))))
        (.catch (fn [e] (js/console.warn "SW registration failed:" e))))))

(defn- uint8array->base64 [ab]
  (bytes->base64 (js/Uint8Array. ab)))

(defn- normalize-base64url [s]
  (let [b64 (-> s (str/trim) (str/replace "-" "+") (str/replace "_" "/"))
        rem (mod (count b64) 4)]
    (case rem
      2 (str b64 "==")
      3 (str b64 "=")
      0 b64
      (throw (js/Error. (str "Invalid base64url length: " (count b64)))))))

(defn- base64url->bytes-safe [s]
  (js/console.log s (count s))
  (base64url->bytes (normalize-base64url s)))
  
(defonce vapid-key (atom nil))

(defn prefetch-vapid-key! []
  (-> (race-request "/push/vapid-key"
                     (fn [url signal] (js/fetch url #js {:signal signal}))
                     0)
      (.then #(.text %))
      (.then #(reset! vapid-key %))
      (.catch (fn [e] (js/console.warn "vapid prefetch failed:" e)))))

(defn subscribe-to-push! [nickname birthdate]
  (when (and (.-serviceWorker js/navigator) (.-PushManager js/window) @vapid-key)
    (-> (.-ready (.-serviceWorker js/navigator))
      (.then (fn [registration]
        (.getSubscription (.-pushManager registration))))
      (.then (fn [existing]
        (if existing
          nil ;; подписка уже есть -- ничего не делаем
          (-> (.-ready (.-serviceWorker js/navigator))
            (.then (fn [registration]
              (.subscribe (.-pushManager registration)
                #js {:userVisibleOnly true :applicationServerKey (base64url->bytes-safe @vapid-key)})))
                  (.then (fn [sub]
                    (race-request "/push/subscribe"
                      (fn [url signal]
                        (js/fetch url
                          #js {:method "POST" :headers #js {"Content-Type" "text/plain; charset=utf-8"}
                          :body (sexp/write [nickname
                            birthdate
                            (.-endpoint sub)
                            (uint8array->base64 (.getKey sub "p256dh"))
                            (uint8array->base64 (.getKey sub "auth"))])
                                :signal signal}))
                            0)))))))
        (.catch (fn [e] (js/console.warn "Push subscribe failed:" e))))))

(defn- open-incoming-call! [nickname]
  (let [me  (:me @state)
        idx (first (keep-indexed (fn [i c] (when (= (first c) nickname) i))
                                  (:contacts me)))]
    (when idx
      (let [bd (second (nth (:contacts me) idx))]
        (open-compose! nickname nil :contact idx)
        (call/call! me nickname bd)))))

;; ?who=nick-Y-M-D-secret&room=nick-Y-M-D&room=...
(defn- parse-who [s]
  (when-let [[_ nick y m d secret] (re-matches #"(.+)-(\d+)-(\d+)-(\d+)-([^-]+)" s)]
    [nick (make-birthdate y m d) secret]))

(defn- parse-room [s]
  (when-let [[_ nick y m d] (re-matches #"(.+)-(\d+)-(\d+)-(\d+)" s)]
    [nick (make-birthdate y m d)]))

(defn- apply-url-params!
  "Возвращает ник для автозвонка (?call=...) или nil."
  []
  (let [params    (js/URLSearchParams. js/location.search)
        who       (.get params "who")
        rooms     (array-seq (.getAll params "room"))
        call-nick (.get params "call")]
    (when (or who (seq rooms) call-nick)
      (.replaceState js/history nil "" (.. js/location -pathname))
      (when (and who (not (:me @state)))
        (when-let [[nick bd secret] (parse-who who)]
          (setup! nick bd secret)))
      (when (:me @state)
        (doseq [r rooms]
          (when-let [[nick bd] (parse-room r)]
            (when-not (find-contact (:me @state) nick)
              (add-contact! nick bd))))))
    call-nick))

(defn init! []
  (theme/apply-colors! @theme/progress)
  (register-service-worker!)
  (let [call-nick (apply-url-params!)]
    (when (:me @state)
      (refresh-inbox!)
      (ensure-identity-keypair!)
      (when call-nick
        (open-incoming-call! call-nick))))
  ;; https://namdel.ru/app?who=kenga-1-1-1-1&room=lain-1992-3-20&call=lain
  ;; аналог onResume() в NamViewModel: при возврате в приложение
  ;; обновляем инбокс (если на главном экране) и возвращаем wake lock
  ;; (если идёт звонок: браузер снимает его при уходе в фон).
  (.addEventListener js/document "visibilitychange"
    (fn []
      (when (= (.-visibilityState js/document) "visible")
        (when (= (:screen @state) :main)
          (refresh-inbox!))
        (when (= (:status @state) :in-call)
          (call/request-wake-lock!)))))
  (rdom/render [root] (.getElementById js/document "app"))
  (prefetch-vapid-key!)
  ;; первый рендер уже прошёл с прошлой сохранённой темой (без вспышки).
  ;; Теперь проверяем текущую фазу луны -- если она требует другой темы,
  ;; проигрываем ту же анимацию, что и при ручном toggle-theme!, чтобы
  ;; пользователь увидел, как его прошлая настройка переписывается лунным
  ;; статусом, а не просто открыл приложение уже с готовым результатом.
  (let [moon-theme (if (moon/waxing?) :light :dark)]
    (when (not= moon-theme @theme/theme)
      (js/setTimeout #(theme/set-theme! moon-theme) 2000))))