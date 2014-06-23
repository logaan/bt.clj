(ns testing-gloss.core
  (:require [gloss.core :refer :all]
            [gloss.io :refer :all]
            [lamina.core :refer :all]
            [aleph.tcp :refer :all]))

(defcodec handshake
  (ordered-map :protocol-name (finite-frame :ubyte (string :us-ascii))
               :reserved      :uint64
               :info-hash     (repeat 20 :ubyte)
               :peer-id       (string :us-ascii :length 20)))

(def pwm
  (array-map
    :choke        (compile-frame {:type     :choke})
    :unchoke      (compile-frame {:type     :unchoke})
    :interested   (compile-frame {:type     :interested})
    :uninterested (compile-frame {:type     :uninterested})
    :have         (compile-frame {:type     :have
                                  :index    :uint32})
    :bitfield     (compile-frame {:type     :bitfield
                                  :bitfield (repeated :ubyte :prefix :none)})
    :request      (compile-frame {:type     :request
                                  :index    :uint32
                                  :offset   :uint32
                                  :length   :uint32})
    :piece        (compile-frame {:type     :piece
                                  :index    :uint32
                                  :offset   :uint32
                                  :block    (repeated :ubyte :prefix :none)})
    :cancel       (compile-frame {:type     :cancel
                                  :index    :uint32
                                  :offset   :uint32
                                  :length   :uint32})))

(defcodec types-enum
  (apply (partial enum :ubyte) (keys pwm)))

(def peer-wire-messages
  (finite-frame :uint32 (header types-enum pwm :type)))

(defn sha1-to-byte-seq [sha1]
  (seq (byte-array (drop 1 (.toByteArray (biginteger sha1))))))

(def handshake-msg
  {:protocol-name "BitTorrent protocol"
   :reserved  0
   :info-hash (sha1-to-byte-seq 0xd8a871a8485f51c2b399f78af161d0fca35b5c46)
   :peer-id   "bt.clj  ------------"})

(defn send-pwm [ch msg]
  (enqueue ch (encode peer-wire-messages msg)))

(defn channel-test []
  (let [ch (wait-for-result
             (tcp-client {:host "localhost" :port 56048}))]
    (try
      (enqueue ch (encode-all handshake [handshake-msg]))
      (let [bbc       (map* #(.toByteBuffer %) ch)
            hc        (decode-channel-headers bbc [handshake])
            handshake @(read-channel hc)
            pwc       (decode-channel hc peer-wire-messages)
            bitfield  @(read-channel pwc)
            _         (send-pwm ch {:type :bitfield
                                    :bitfield [0]})
            _         (send-pwm ch {:type :interested})
            _         (send-pwm ch {:type :unchoke}) 
            _         (send-pwm ch {:type   :request
                                    :index  0
                                    :offset 0
                                    :length 11232})]
        (println handshake)
        (println bitfield))
      (finally (force-close ch)))))

; Transmission isn't sending the piece. Don't know why. Probably an error in
; either index, offset or length of the request msg. Should hook up a second
; torrent app and get them talking to each other to see what a req looks like.

(comment

  (channel-test)

  )

