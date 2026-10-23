(ns nam.call
  (:require [nam.sexp :as sexp]
            [reagent.core :as r]
            [nam.port :refer [ice-config relay-ice-config]]
            [nam.ico :as ico]))

(declare handle-call-message! schedule-reconnect! restart-via-relay! ensure-online-listener! schedule-in-call-reconnect!)

;; ---------------------------------------------------------------------
;; State
;; ---------------------------------------------------------------------

(defonce state (r/atom {:ws nil
                       :pc nil
                       :connecting nil       ; in-flight connect promise (dedupes concurrent ensure-connected!)
                       :auto-reconnect false ; set by connect!; lazy mode (call! only) never auto-reconnects
                       :reconnect-attempts 0
                       :stats-timer nil      ; setInterval id for connection-path logging
                       :disconnect-timer nil ; setTimeout id for the "disconnected" grace period (see schedule-disconnect-check!)
                       :in-call-reconnect-attempts 0 ; count of relay-restart retries during an outage (backoff, see schedule-in-call-reconnect!)
                       :in-call-reconnect-timer nil  ; setTimeout id for the pending retry above
                       :local-stream nil     ; mic MediaStream -- must be stopped explicitly to release the mic
                       :self nil
                       :peer nil
                       :pending-offer nil    ; {:nickname .. :birthdate .. :sdp ..}
                       :pending-ice []       ; ICE candidate JSON strings queued until remoteDescription is set
                       :status nil           ; nil | :calling | :in-call
                       :on-status-change nil ; (fn [status peer-nick])
                       :presence-resolve nil
                       :on-unavailable nil
                       :is-offerer false      ; true if we sent the (last) offer -- only the offerer drives rtt-fallback renegotiation
                       :ice-fallback-attempts 0 ; caps retries of the p2p->relay switch (see log-selected-pair!)
                       :force-relay false})) ; true after restart-via-relay!: new pc is built relay-only (both sides)

;; ---------------------------------------------------------------------
;; Manual sexp construction (nam.sexp/write has no keyword support)
;; ---------------------------------------------------------------------

(defn- to-event [to-nick to-bd payload-sexp]
  (str "(:to " (sexp/write to-nick) " " (sexp/write to-bd) " :payload " payload-sexp ")"))

(defn- offer-payload [from-nick from-bd sdp]
  (str "(:call/offer " (sexp/write from-nick) " " (sexp/write from-bd) " " (sexp/write sdp) ")"))

(defn- answer-payload [sdp]
  (str "(:call/answer " (sexp/write sdp) ")"))

;; Sent by the offerer after it tore down its pc and built a new relay-only one.
;; Distinct from :call/offer on purpose: the new pc has a new DTLS fingerprint,
;; which can't be applied to the peer's live pc, so the receiver must rebuild
;; its pc too (see handle-reoffer!).
(defn- reoffer-payload [sdp]
  (str "(:call/reoffer " (sexp/write sdp) ")"))

(defn- ice-payload [json-str]
  (str "(:call/ice " (sexp/write json-str) ")"))

(defn- end-payload [] "(:call/end)")

;; ---------------------------------------------------------------------
;; Call-WS connection -- opened by connect! at startup (or lazily on first call)
;; ---------------------------------------------------------------------

(defn- ensure-connected! [me]
  (cond
    (:ws @state) (js/Promise.resolve (:ws @state))
    (:connecting @state) (:connecting @state)
    :else
    (let [p (js/Promise.
             (fn [resolve reject]
               (let [ws (js/WebSocket. "wss://namdel.ru/call-ws")]
                 (set! (.-onopen ws)
                       (fn []
                         (.send ws (sexp/write (list (:nickname me) (:birthdate me) (:secret me))))))
                 (set! (.-onmessage ws)
                       (fn [e]
                         (let [msg (sexp/read (.-data e))]
                           (if (= (first msg) ":OK")
                             (do
                               (swap! state assoc :ws ws :self me :reconnect-attempts 0)
                               (set! (.-onmessage ws) #(handle-call-message! (.-data %)))
                               (resolve ws))
                             (do
                               ;; bad credentials -- retrying would just spin
                               (swap! state assoc :auto-reconnect false)
                               (reject (js/Error. (str "call-ws auth failed: " (pr-str msg)))))))))
                 (set! (.-onerror ws) (fn [e] (reject e)))
                 (set! (.-onclose ws)
                       (fn [_]
                         (swap! state assoc :ws nil)
                         (reject (js/Error. "call-ws closed before auth")) ; no-op if already resolved
                         (schedule-reconnect! me))))))]
      (swap! state assoc :connecting p)
      (.then p
             #(swap! state assoc :connecting nil)
             #(swap! state assoc :connecting nil))
      p)))

;; Exponential backoff (1s, 2s, 4s ... capped at 30s) plus random jitter, so a
;; server restart doesn't make every client reconnect at the same instant.
(defn- schedule-reconnect! [me]
  (when (:auto-reconnect @state)
    (let [n     (:reconnect-attempts @state)
          delay (+ (min 30000 (* 1000 (js/Math.pow 2 n))) (rand-int 1000))]
      (swap! state update :reconnect-attempts inc)
      (js/setTimeout
       (fn []
         (when (and (:auto-reconnect @state) (nil? (:ws @state)))
           (-> (ensure-connected! me) (.catch (fn [_] nil)))))
       delay))))

(defn connect!
  "Call once after login: opens call-ws right away and keeps it alive."
  [me]
  (ensure-online-listener!)
  (swap! state assoc :auto-reconnect true :reconnect-attempts 0)
  (-> (ensure-connected! me)
      (.catch (fn [e] (js/console.warn "call-ws connect failed:" e)))))

(defn- check-presence! [to-nick to-bd]
  (js/Promise.
   (fn [resolve reject]
     (if-let [ws (:ws @state)]
       (do
         (swap! state assoc :presence-resolve resolve)
         (.send ws (str "(:presence " (sexp/write to-nick) " " (sexp/write to-bd) ")")))
       (reject (js/Error. "no ws"))))))

;; Previously this silently dropped the payload whenever (:ws @state) was nil
;; (e.g. call-ws still reconnecting after a network blip) -- exactly the kind
;; of narrow window where a reoffer/answer/ICE candidate is most likely to be
;; sent, since that's when a reconnect attempt is in flight. Now it waits for
;; ensure-connected! (which kicks off an immediate connect attempt if none is
;; in flight, or piggybacks on one already running) before sending. Callers
;; don't await this -- it's fire-and-forget, same as before.
(defn- send-call-event! [to-nick to-bd payload-sexp]
  (when-let [me (:self @state)]
    (-> (ensure-connected! me)
        (.then (fn [ws] (.send ws (to-event to-nick to-bd payload-sexp))))
        (.catch (fn [e] (js/console.warn "[call] send-call-event! failed:" e))))))

(defonce wake-lock (atom nil))

(defn request-wake-lock! []
  (when (and (not @wake-lock)
             js/navigator
             (.-wakeLock js/navigator))
    (-> (.request (.-wakeLock js/navigator) "screen")
        (.then (fn [lock]
                 (reset! wake-lock lock)
                 (.addEventListener lock "release"
                                    (fn [_] (reset! wake-lock nil)))))
        (.catch (fn [err]
                  (js/console.warn "Wake lock failed:" err))))))

(defn- release-wake-lock! []
  (when-let [lock @wake-lock]
    (.release lock)
    (reset! wake-lock nil)))

(defn- set-status! [status peer-nick]
  (swap! state assoc :status status)
  (if (= status :in-call)
    (request-wake-lock!)
    (release-wake-lock!))
  (when-let [cb (:on-status-change @state)] (cb status peer-nick)))

;; pc.close() does NOT stop local tracks -- without this the browser keeps
;; the mic open (red recording dot on the tab) after the call ends.
(defn- stop-local-stream! []
  (when-let [s (:local-stream @state)]
    (doseq [t (array-seq (.getTracks s))] (.stop t))
    (swap! state assoc :local-stream nil)))

;; ---------------------------------------------------------------------
;; Connection-path logging (console): which candidate pair carries the call,
;; over what protocol, and how good it is. Same info as chrome://webrtc-internals.
;; ---------------------------------------------------------------------

(defn- report->seq [report]
  (array-seq (js/Array.from (.values report))))

(defn- log-selected-pair! [pc]
  (-> (.getStats pc)
      (.then
       (fn [report]
         (try
           (let [stats   (report->seq report)
                 by-type (fn [t] (filter #(= (.-type %) t) stats))
                 pair-id (some #(.-selectedCandidatePairId %) (by-type "transport"))
                 pair    (or (when pair-id (.get report pair-id))
                             ;; Firefox has no selectedCandidatePairId
                             (first (filter #(and (.-nominated %) (= (.-state %) "succeeded"))
                                            (by-type "candidate-pair"))))]
             (if-not pair
               (js/console.log "[call] no selected candidate pair yet")
               (let [local        (.get report (.-localCandidateId pair))
                     remote       (.get report (.-remoteCandidateId pair))
                     audio        (first (filter #(= (.-kind %) "audio") (by-type "inbound-rtp")))
                     remote-audio (first (filter #(= (.-kind %) "audio") (by-type "remote-inbound-rtp")))
                     rtt          (.-currentRoundTripTime pair)
                     rtcp-rtt     (when remote-audio (.-roundTripTime remote-audio))
                     jbd          (when audio (.-jitterBufferDelay audio))
                     jbe          (when audio (.-jitterBufferEmittedCount audio))
                     switched?    (not= (.-id pair) (:last-pair-id @state))]
                 (swap! state assoc :last-pair-id (.-id pair))
                 (js/console.log
                  (str "[call] " (when switched? "(SWITCHED) ")
                       ":" (.-candidateType local)
                       ;; relayProtocol = client->TURN leg (udp/tcp/tls); only on relay candidates
                       (when (.-relayProtocol local) (str "(" (.-relayProtocol local) ")"))
                       " :" (.-candidateType remote)
                       (when rtt (str " ice:" (js/Math.round (* 1000 rtt)) "ms"))
                       (when rtcp-rtt (str " rtcp:" (js/Math.round (* 1000 rtcp-rtt)) "ms"))
                       ;; средняя задержка плейбэк-буфера за всё время звонка (накопительная сумма /
                       ;; счётчик, а не мгновенное значение) -- это то, что реально слышно как лаг
                       (when (and jbd jbe (pos? jbe))
                         (str " avg:" (js/Math.round (* 1000 (/ jbd jbe))) "ms"))
                       ;" gone " (.-bytesSent pair)
                       ;" recv " (.-bytesReceived pair) "B"
                       (when audio (str " j:" (js/Math.round (* 1000 (.-jitter audio))) "ms"
                                        " dead:" (.-packetsLost audio)))
                       " " (.-address local) ":" (.-port local)
                       " <-> " (.-address remote) ":" (.-port remote)))
                 )))
           (catch :default e
             (js/console.warn "[call] stats parse failed:" e)))))
      (.catch (fn [e] (js/console.warn "[call] getStats failed:" e)))))

(defn- stop-stats! []
  (when-let [t (:stats-timer @state)]
    (js/clearInterval t)
    (swap! state assoc :stats-timer nil)))

(defn- start-stats! [pc]
  (stop-stats!)
  (js/console.log "[call] iceTransportPolicy:" (.-iceTransportPolicy ice-config))
  (log-selected-pair! pc)
  (swap! state assoc :stats-timer
         (js/setInterval
          (fn []
            (if (= (.-connectionState pc) "closed")
              (stop-stats!)
              (log-selected-pair! pc)))
          5000)))

(defn- clear-disconnect-timer! []
  (when-let [t (:disconnect-timer @state)]
    (js/clearTimeout t)
    (swap! state assoc :disconnect-timer nil)))

;; iceConnectionState "disconnected" is often transient (brief network blip)
;; and the browser frequently recovers on its own within a few seconds --
;; restarting immediately would cause needless relay-restarts. Give it a
;; grace period and only restart if it's still stuck after that. Only the
;; offerer restarts (same glare-avoidance reasoning as elsewhere); the guard
;; `(= pc (:pc @state))` skips a stale timer firing after the pc was already
;; replaced or torn down.
(defn- schedule-disconnect-check! [pc]
  (clear-disconnect-timer!)
  (swap! state assoc :disconnect-timer
         (js/setTimeout
          (fn []
            (swap! state assoc :disconnect-timer nil)
            (when (and (= pc (:pc @state))
                       (= (.-iceConnectionState pc) "disconnected")
                       (:is-offerer @state))
              (js/console.log "[call] still disconnected after grace period -- retrying")
              (schedule-in-call-reconnect! pc)))
          3000)))

(defn- clear-in-call-reconnect-timer! []
  (when-let [t (:in-call-reconnect-timer @state)]
    (js/clearTimeout t)
    (swap! state assoc :in-call-reconnect-timer nil)))

;; A single restart-via-relay! call is not enough during a real outage: the
;; relay attempt itself can fail while the network is still down (no route to
;; any STUN/TURN server yet). Retry with backoff (immediately, then 1s, 2s,
;; 4s ... capped at 20s) until the call recovers (attempts reset to 0 on
;; "connected", see onconnectionstatechange) or the call ends (reset-call!/
;; restart-via-relay! clear the timer). Offerer-only, like the rest of the
;; relay-restart machinery, to avoid both sides restarting at once.
(defn- schedule-in-call-reconnect! [pc]
  (clear-in-call-reconnect-timer!)
  (let [n     (:in-call-reconnect-attempts @state)
        delay (if (zero? n) 0 (min 20000 (* 1000 (js/Math.pow 2 (dec n)))))]
    (swap! state update :in-call-reconnect-attempts inc)
    (swap! state assoc :in-call-reconnect-timer
           (js/setTimeout
            (fn []
              (swap! state assoc :in-call-reconnect-timer nil)
              (when (and (= pc (:pc @state))
                         (:is-offerer @state)
                         (not (#{"connected" "completed"} (.-iceConnectionState pc))))
                (js/console.log "[call] reconnect attempt" (inc n) "-- restarting via relay")
                (restart-via-relay! pc)))
            delay))))

;; ICE candidates can arrive over the ws before setRemoteDescription has
;; finished (or even before the RTCPeerConnection exists yet, e.g. while
;; we're still waiting on getUserMedia). Queue them and flush once the
;; remote description is actually set -- calling addIceCandidate before
;; that throws InvalidStateError: "remote description was null".
(defn- flush-pending-ice! [pc]
  (let [pending (:pending-ice @state)]
    (swap! state assoc :pending-ice [])
    (doseq [c pending]
      (-> (.addIceCandidate pc (.parse js/JSON c))
          (.catch (fn [e] (js/console.warn "addIceCandidate (flushed) failed:" e)))))))

;; ---------------------------------------------------------------------
;; RTCPeerConnection setup
;; ---------------------------------------------------------------------

(defn- make-peer-connection! [to-nick to-bd]
  (let [pc (js/RTCPeerConnection. (if (:force-relay @state) relay-ice-config ice-config))]
    (set! (.-onicecandidate pc)
          (fn [e]
            (when-let [cand (.-candidate e)]
              (js/console.log "[call debug] local candidate gathered, type=" (.-type cand)
                               "protocol=" (.-protocol cand))
              (send-call-event! to-nick to-bd
                                 (ice-payload (.stringify js/JSON (.toJSON cand)))))))
    ;; TEMP DEBUG: STUN/TURN-специфичные ошибки сбора кандидатов (401 = битые
    ;; креды, timeout/unreachable = сервер/порт недоступен и т.п.) -- fires
    ;; per gathering attempt, including on ICE restarts, and reject/catch on
    ;; createOffer/setLocalDescription never sees these.
    (set! (.-onicecandidateerror pc)
          (fn [e]
            (js/console.warn "[call] icecandidateerror" (.-errorCode e) (.-errorText e)
                              "url=" (.-url e) "address=" (.-address e) "port=" (.-port e))))
    (set! (.-ontrack pc)
      (fn [e]
        (js/console.log "ontrack fired" e (.-streams e))
        (let [stream (aget (.-streams e) 0)
              tracks (.getAudioTracks stream)]
          (js/console.log "audio tracks:" tracks)
          (doseq [t (array-seq tracks)]
            (js/console.log "track enabled/muted/readyState:"
                             (.-enabled t) (.-muted t) (.-readyState t)))
          (let [audio (js/Audio.)]
            (set! (.-srcObject audio) stream)
            (-> (.play audio)
                (.then #(js/console.log "play() resolved, paused=" (.-paused audio)))
                (.catch (fn [err] (js/console.error "play failed:" err))))))))
    (set! (.-onconnectionstatechange pc)
          (fn []
            (when (= (.-connectionState pc) "connected")
              (clear-in-call-reconnect-timer!)
              (swap! state assoc :in-call-reconnect-attempts 0)
              (set-status! :in-call to-nick)
              (start-stats! pc))))
    (set! (.-oniceconnectionstatechange pc)
          (fn []
            (let [ice-state (.-iceConnectionState pc)]
              (js/console.log "[call] ice state:" ice-state)
              (case ice-state
                "connected"    (clear-disconnect-timer!)
                "disconnected" (schedule-disconnect-check! pc)
                "failed"       (do
                                 (clear-disconnect-timer!)
                                 ;; only the offerer restarts (glare); guard against
                                 ;; a stale event from an already-replaced pc
                                 (when (and (= pc (:pc @state)) (:is-offerer @state))
                                   (js/console.log "[call] ICE failed -- retrying")
                                   (schedule-in-call-reconnect! pc)))
                nil))))
    ;; NB: don't reset :pending-ice here -- the caller's ICE candidates may
    ;; already have been queued while we were waiting for the user to answer.
    (swap! state assoc :pc pc)
    pc))

;; Mid-call switch to tcp relay: tear the pc down and build a new one,
;; relay-only (:force-relay -> relay-ice-config, whose TURN url is tcp by
;; default, see turn-transports). The mic is NOT re-acquired: the same
;; local-stream tracks are added to the new pc. Expect ~1-3s of silence while
;; the TURN allocation + DTLS handshake complete.
;; The receiver rebuilds its pc too when it gets :call/reoffer (handle-reoffer!),
;; and is also forced to relay -- otherwise its RTP would go over udp straight
;; to the TURN relay address, which is exactly the traffic that gets shaped.
;; Only called by the offerer side (see the :is-offerer gate in
;; log-selected-pair!) to avoid both peers restarting at once (glare).
(defn- restart-via-relay! [old-pc]
  (let [{:keys [nickname birthdate]} (:peer @state)
        local-stream (:local-stream @state)
        me           (:self @state)]
    (stop-stats!)
    (clear-disconnect-timer!)
    (.close old-pc)
    (swap! state assoc :force-relay true :pending-ice [])
    (let [pc (make-peer-connection! nickname birthdate)]
      (doseq [t (array-seq (.getTracks local-stream))] (.addTrack pc t local-stream))
      ;; ICE gathering (and therefore onicecandidate -> send-call-event!) only
      ;; starts after setLocalDescription below, so gating createOffer on
      ;; ensure-connected! also delays candidate gathering until call-ws is
      ;; actually back -- otherwise send-call-event! silently no-ops while
      ;; (:ws @state) is nil and the reoffer/candidates are lost for good.
      (-> (ensure-connected! me)
          (.then (fn [_] (.createOffer pc)))
          (.then (fn [offer]
                   (-> (.setLocalDescription pc offer)
                       (.then #(do
                                 (js/console.log "[call] relay restart: reoffer sent, waiting on answer")
                                 (send-call-event! nickname birthdate
                                                   (reoffer-payload (.-sdp offer))))))))
          (.catch (fn [e]
                    (js/console.error "restart via relay failed:" e)
                    (when (= pc (:pc @state))
                      (schedule-in-call-reconnect! pc))))))))

;; The browser only tells us the network is *gone* via ICE state changes
;; (disconnected/failed come with no useful timing info on when it'll come
;; back). It DOES tell us when it's back, via the 'online' event -- use that
;; to short-circuit the backoff delay instead of waiting it out blindly.
(defonce ^:private online-listener-added? (atom false))

(defn- ensure-online-listener! []
  (when-not @online-listener-added?
    (reset! online-listener-added? true)
    (.addEventListener js/window "online"
                        (fn []
                          (when-let [pc (:pc @state)]
                            (when (and (:is-offerer @state)
                                       (#{"failed" "disconnected"} (.-iceConnectionState pc)))
                              (js/console.log "[call] network back online -- retrying now")
                              (clear-in-call-reconnect-timer!)
                              (swap! state assoc :in-call-reconnect-attempts 0)
                              (restart-via-relay! pc)))))))

;; ---------------------------------------------------------------------
;; The single "call" button action:
;; answers a pending offer from this contact if there is one,
;; otherwise places an outgoing call to them.
;; ---------------------------------------------------------------------

(defn call! [me contact-nick contact-bd]
  (-> (ensure-connected! me)
      (.then
       (fn [_]
         (let [pending (:pending-offer @state)]
           (if (and pending (= (:nickname pending) contact-nick))
             ;; they already called us -- answer
             (let [sdp (:sdp pending)]
               (swap! state assoc :peer {:nickname contact-nick :birthdate contact-bd} :pending-offer nil
                      :is-offerer false :ice-fallback-attempts 0 :force-relay false
                      :in-call-reconnect-attempts 0)
               (set-status! :calling contact-nick)
               ;; release the previous attempt's pc + mic before starting a new one
               (when-let [old (:pc @state)] (.close old))
               (stop-local-stream!)
               (-> (.getUserMedia (.-mediaDevices js/navigator) #js {:audio true})
                   (.then
                    (fn [local-stream]
                      (swap! state assoc :local-stream local-stream)
                      (let [pc (make-peer-connection! contact-nick contact-bd)]
                        (doseq [t (array-seq (.getTracks local-stream))] (.addTrack pc t local-stream))
                        (-> (.setRemoteDescription pc #js {:type "offer" :sdp sdp})
                            (.then (fn [_]
                                     (flush-pending-ice! pc)
                                     (.createAnswer pc)))
                            (.then (fn [answer]
                                     (-> (.setLocalDescription pc answer)
                                         (.then #(send-call-event!
                                                  contact-nick contact-bd
                                                  (answer-payload (.-sdp answer)))))))))))))
             ;; no pending offer from them -- place a fresh call
              (-> (check-presence! contact-nick contact-bd)
                  (.then
                  (fn [online?]
                    (if-not online?
                      (do
                       (swap! state assoc :peer {:nickname contact-nick :birthdate contact-bd})
                       (set-status! :waiting contact-nick))
                      ;(when-let [cb (:on-unavailable @state)] (cb))
                      (do
                        (swap! state assoc :peer {:nickname contact-nick :birthdate contact-bd} :pending-ice []
                               :is-offerer true :ice-fallback-attempts 0 :force-relay false
                               :in-call-reconnect-attempts 0)
                        (set-status! :calling contact-nick)
                        (when-let [old (:pc @state)] (.close old))
                        (stop-local-stream!)
                        (-> (.getUserMedia (.-mediaDevices js/navigator) #js {:audio true})
                            (.then
                              (fn [local-stream]
                                (swap! state assoc :local-stream local-stream)
                                (let [pc (make-peer-connection! contact-nick contact-bd)]
                                  (doseq [t (array-seq (.getTracks local-stream))] (.addTrack pc t local-stream))
                                  (-> (.createOffer pc)
                                      (.then (fn [offer]
                                              (-> (.setLocalDescription pc offer)
                                                  (.then #(send-call-event!
                                                            contact-nick contact-bd
                                                            (offer-payload (:nickname me) (:birthdate me) (.-sdp offer)))))))))))))))))
             ))))
      (.catch (fn [e] (js/console.error "call! failed:" e)))))

;; ---------------------------------------------------------------------
;; Incoming events
;; ---------------------------------------------------------------------

(defn- handle-offer! [from-nick from-bd sdp]
  (swap! state assoc :pending-offer {:nickname from-nick :birthdate from-bd :sdp sdp})
  (when (and (= (:status @state) :waiting)
             (= (:nickname (:peer @state)) from-nick))
    (when-let [me (:self @state)]
      (call! me from-nick from-bd))))

;; The peer tore its pc down and rebuilt it relay-only (restart-via-relay!).
;; Do the same on our side: close the old pc, build a new relay-only one on the
;; SAME mic stream (no getUserMedia) and answer the new offer.
(defn- handle-reoffer! [sdp]
  (when-let [{:keys [nickname birthdate]} (:peer @state)]
    (when-let [local-stream (:local-stream @state)]
      (stop-stats!)
      (clear-disconnect-timer!)
      (clear-in-call-reconnect-timer!)
      (when-let [old (:pc @state)] (.close old))
      (swap! state assoc :force-relay true :pending-ice [])
      (let [pc (make-peer-connection! nickname birthdate)]
        (doseq [t (array-seq (.getTracks local-stream))] (.addTrack pc t local-stream))
        (-> (.setRemoteDescription pc #js {:type "offer" :sdp sdp})
            (.then (fn [_]
                     (flush-pending-ice! pc)
                     (.createAnswer pc)))
            (.then (fn [answer]
                     (-> (.setLocalDescription pc answer)
                         (.then #(send-call-event! nickname birthdate
                                                   (answer-payload (.-sdp answer)))))))
            (.catch (fn [e] (js/console.error "reoffer answer failed:" e))))))))

(defn- handle-answer! [sdp]
  (when-let [pc (:pc @state)]
    (-> (.setRemoteDescription pc #js {:type "answer" :sdp sdp})
        (.then #(do
                  (js/console.log "[call] answer applied, new ICE checks starting")
                  (flush-pending-ice! pc)))
        (.catch (fn [e] (js/console.error "setRemoteDescription (answer) failed:" e))))))

(defn- handle-ice! [candidate-json]
  (let [pc (:pc @state)]
    (if (and pc (.-remoteDescription pc))
      (-> (.addIceCandidate pc (.parse js/JSON candidate-json))
          (.catch (fn [e] (js/console.warn "addIceCandidate failed:" e))))
      (swap! state update :pending-ice conj candidate-json))))

(defn- reset-call!
  "Local teardown only -- sends nothing to the peer."
  []
  (stop-stats!)
  (clear-disconnect-timer!)
  (clear-in-call-reconnect-timer!)
  (when-let [pc (:pc @state)] (.close pc))
  (stop-local-stream!)
  (swap! state assoc :pc nil :peer nil :pending-offer nil :pending-ice []
         :is-offerer false :ice-fallback-attempts 0 :force-relay false
         :in-call-reconnect-attempts 0)
  (set-status! nil nil))

(defn- handle-remote-end! []
  (reset-call!))

(defn- handle-unavailable! []
  (reset-call!)
  (when-let [cb (:on-unavailable @state)] (cb)))

(defn- handle-call-message! [raw]
  (let [msg (sexp/read raw)
        tag (first msg)]
    (cond
      (= tag ":PRESENCE/ONLINE")  (when-let [r (:presence-resolve @state)] (r true) (swap! state dissoc :presence-resolve))
      (= tag ":PRESENCE/OFFLINE") (when-let [r (:presence-resolve @state)] (r false) (swap! state dissoc :presence-resolve))
      (= tag ":CALL/OFFER")  (let [[_ n bd sdp] msg] (handle-offer! n bd sdp))
      (= tag ":CALL/REOFFER") (handle-reoffer! (second msg))
      (= tag ":CALL/ANSWER") (handle-answer! (second msg))
      (= tag ":CALL/ICE")    (handle-ice! (second msg))
      (= tag ":CALL/END")    (handle-remote-end!)
      (= tag ":CALL/UNAVAILABLE") (handle-unavailable!)
      :else (js/console.warn "unhandled call message:" (pr-str msg)))))

;; ---------------------------------------------------------------------
;; Wiring + teardown
;; ---------------------------------------------------------------------

#_(defn set-handlers! [{:keys [on-status-change on-unavailable]}]
  (swap! state assoc :on-status-change on-status-change :on-unavailable on-unavailable))

(defn end-call! []
  (when-let [{:keys [nickname birthdate]} (:peer @state)]
    (send-call-event! nickname birthdate (end-payload)))
  (reset-call!))

;; ---------------------------------------------------------------------
;; UI: call button (Call -> Calling/Cancel -> Hang up)
;; ---------------------------------------------------------------------

;; No login step in the app, so the first rendered call button (which receives
;; `me`) kicks off the persistent connection. Runs once; setTimeout keeps the
;; side effect out of the render pass.
(defonce ^:private connect-requested? (atom false))

(defn- ensure-started! [me]
  (when (and me (not @connect-requested?))
    (reset! connect-requested? true)
    (js/setTimeout #(connect! me) 0)))

(defn call-button [me nickname bd ru?]
  (ensure-started! me)
  (let [{:keys [status peer pending-offer]} @state
        mine?     (= (:nickname peer) nickname)
        incoming? (= (:nickname pending-offer) nickname)]
    (cond
      ;; этот контакт нам звонит -- поднять трубку (call! отвечает на pending-offer)
      (and incoming? (not= status :in-call) (or (nil? status) (= status :waiting) mine?))
      [:span {:on-click #(call! me nickname bd)
              :style {:cursor "pointer" :color "#2a2"}}
       "📲 Answer"]

      (and mine? (= status :in-call))
      [:span {:on-click #(end-call!)
              :style {:cursor "pointer" :color "#d33"}}
       "🔴 Hang up"]

      (and mine? (= status :calling))
      [:span {:on-click #(end-call!)
              :style {:cursor "pointer"}}
       "⏳ Calling… ✖ Cancel"]

      (and mine? (= status :waiting))
      [:span {:on-click #(reset-call!)
              :style {:cursor "pointer"}}
       "⏳ Waiting… ✖ Cancel"]

      :else
      [:span
        (cond->
          {:style {:display "inline-flex" :align-items "center" :gap 4}}
          status (assoc-in [:style :opacity] 0.4)
          (not status) (->
            (assoc :on-click #(call! me nickname bd))
            (assoc-in [:style :cursor] "pointer")))
        (if ru? "Звонок" "Call") [ico/bell]])))