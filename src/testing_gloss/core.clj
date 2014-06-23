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

(def test-peer-wire-messages
  [{:type :choke}
   {:type :unchoke}
   {:type :interested}
   {:type :uninterested}])

(defn channel-test []
  (let [ch (wait-for-result
             (tcp-client {:host "localhost" :port 56048}))]
    (enqueue ch (encode-all handshake [handshake-msg]))
    (enqueue ch (encode-all peer-wire-messages test-peer-wire-messages))
    (future (mapv println (channel->lazy-seq ch)))
    (Thread/sleep 1000)
    (close ch)))

(channel-test)

