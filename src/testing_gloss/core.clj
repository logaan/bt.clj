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
    :request      (compile-frame (ordered-map :type     :request
                                              :index    :uint32
                                              :offset   :uint32
                                              :length   :uint32))
    :piece        (compile-frame (ordered-map :type     :piece
                                              :index    :uint32
                                              :offset   :uint32
                                              :block    (repeated :ubyte :prefix :none)))
    :cancel       (compile-frame (ordered-map :type     :cancel
                                              :index    :uint32
                                              :offset   :uint32
                                              :length   :uint32))))

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

(defn request-epl-txt []
  (let [ch (wait-for-result
             (tcp-client {:host "localhost" :port 56048}))]
    (try
      (enqueue ch (encode-all handshake [handshake-msg]))
      (let [byte-buffer-ch (map* #(.toByteBuffer %) ch)
            header-ch      (decode-channel-headers byte-buffer-ch [handshake])
            handshake      @(read-channel header-ch)
            peer-wire-ch   (decode-channel header-ch peer-wire-messages)
            bitfield       @(read-channel peer-wire-ch)
            _              (send-pwm ch {:type :bitfield
                                         :bitfield [0]})
            _              (send-pwm ch {:type :interested})
            _              (send-pwm ch {:type :unchoke}) 
            unchoke        @(read-channel peer-wire-ch)
            _              (send-pwm ch {:type   :request
                                         :index  0
                                         :offset 0
                                         :length 0x00002be0})
            piece   @(read-channel peer-wire-ch)]
        (with-open [f (clojure.java.io/output-stream "output.txt")]
          (.write f (byte-array (map byte (:block piece))))))
      (finally (force-close ch)))))

(comment

  (request-epl-txt)

  )

