(ns nam.port)

;; RESOLVED (2026-09-27/28): the ~300-700ms RTT seen consistently across many
;; real user pairs (different cities/countries), on BOTH direct p2p-udp and
;; udp-relay, while tcp paths stayed <100ms and raw ICMP/iperf3 to the same
;; hosts was clean, is mid-path shaping/deprioritization of UDP traffic that
;; fingerprints as RTP -- confirmed by: (a) plain tcp relay (no TLS) already
;; gives 14-18ms on a pair that got 600-700ms over udp; (b) moving the coturn
;; alt-listener to udp/443 made no difference before we rolled it back (so
;; it's the packet pattern being shaped, not the port). TLS didn't turn out
;; to be necessary, but the TLS listener (turns:turn.namdel.ru:5349, real
;; cert) is left running as a documented fallback for networks that block
;; non-tcp/non-443 outright.
;;
;; Default is now tcp-only: udp relay reliably loses to tcp for these users,
;; so offering it by default just wastes an ICE candidate-pair check and a
;; relay allocation slot (server only has total-quota=20). ?turn=udp is kept
;; for deliberately re-testing/comparing against the udp path.
(defn turn-transports []
  (let [q (re-find #"turn=(tcp|udp|tls)" js/location.search)]
    (case (second q)
      "udp" ["turn:namdel.ru:3478?transport=udp"]
      "tls" ["turns:turn.namdel.ru:5349?transport=tcp"]
      ["turn:namdel.ru:3478?transport=tcp"])))

(def ice-config
  #js {:iceServers #js [#js {:urls #js ["stun:namdel.ru:3478"]}
                        #js {:urls (clj->js (turn-transports))
                             :username "nam"
                             :credential "cuFrqPQyCOMRgiATW8BuITUWiPcVAzYE"}]
        ;; Test switch: open the app with ?relay=1 to force all media through TURN.
        ;; If a call still has sound, the relay ports (49160-49200 udp) are reachable.
        :iceTransportPolicy (if (re-find #"relay=1" js/location.search) "relay" "all")})


;; Config for the pc rebuilt by restart-via-relay!: same servers, relay only.
(def relay-ice-config
  #js {:iceServers (.-iceServers ice-config)
       :iceTransportPolicy "all"})