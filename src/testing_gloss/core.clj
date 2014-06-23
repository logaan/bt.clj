(ns testing-gloss.core
  (:require [gloss.core :refer :all]
            [gloss.io :refer :all]
            [lamina.core :refer :all]
            [aleph.tcp :refer :all])
  (:import java.io.FileOutputStream
           java.net.InetSocketAddress
           java.nio.channels.SocketChannel
           java.io.BufferedOutputStream))

; Peer wire protocol

(defmacro defpeer-wire-msg [msg-type & fields]
  (let [type-kw (keyword msg-type)]
    `(defcodec ~msg-type
       (ordered-map :type ~type-kw ~@ fields))))

(defpeer-wire-msg choke)
(defpeer-wire-msg unchoke)
(defpeer-wire-msg interested)
(defpeer-wire-msg uninterested)

(defpeer-wire-msg have
  :index :uint32)

(defpeer-wire-msg bitfield 
  :bitfield (repeated :ubyte :prefix :none))

(defpeer-wire-msg request
  :index   :uint32
  :offset  :uint32
  :length  :uint32)

(defpeer-wire-msg piece
  :index   :uint32
  :offset  :uint32
  :block   (repeated :ubyte :prefix :none))

(defpeer-wire-msg cancel
  :index   :uint32
  :offset  :uint32
  :length  :uint32)

(def peer-wire-codecs
  '[choke unchoke interested uninterested have bitfield request piece cancel])

(def type->codec
  (apply array-map (mapcat (juxt keyword eval) peer-wire-codecs))) 

(defcodec types-enum
  (apply (partial enum :ubyte) (map keyword peer-wire-codecs)))

(def peer-wire-messages
  (finite-frame :uint32 (header types-enum type->codec :type)))

(defcodec handshake
  (ordered-map :protocol-name (finite-frame :ubyte (string :us-ascii))
               :reserved      :uint64
               :info-hash     (repeat 20 :ubyte)
               :peer-id       (string :us-ascii :length 20)))

; Util

(defn while-connected-to [hostname port f]
  (let [address (InetSocketAddress. hostname port)]
    (with-open [socket-channel (SocketChannel/open address)]
      (let [socket (.socket socket-channel)]
        (f (.getInputStream socket)
           (BufferedOutputStream. (.getOutputStream socket)))))))

(defn sha1-to-byte-seq [sha1]
  (seq (byte-array (drop 1 (.toByteArray (biginteger sha1))))))

; Test

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
            _         (send-pwm ch {:type :interested})
            _         (send-pwm ch {:type   :request
                                    :index  1
                                    :offset 0
                                    :length 1})
            _         (send-pwm ch {:type :unchoke})]
        (println handshake)
        (println bitfield)
        (Thread/sleep 2000)) 
      (finally (force-close ch)))))

; With lengthed .toByteBuffer and seq it works
;
; With no lengthed .toByteBuffer the error we get is:
; ClassCastException org.jboss.netty.buffer.BigEndianHeapChannelBuffer cannot be cast to java.nio.Buffer  gloss.data.bytes.core/create-buf-seq (core.clj:254)
;
; With lengthed .toByteBuffer but no sequence it works
;
; Without lengthed .toByteArray or a seq the error we get is:
; IllegalArgumentException Don't know how to create ISeq from: org.jboss.netty.buffer.BigEndianHeapChannelBuffer  clojure.lang.RT.seqFrom (RT.java:505)
;
; With non-lengthed .toByteBuffer the exception we get is:
; Exception Bytes left over after decoding frame.   gloss.io/decode (io.clj:86)
; But only when the bitfield is sent through in the same packet.
; Otherwise it works fine.

(comment

  (channel-test)

  )

