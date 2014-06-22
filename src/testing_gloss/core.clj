(ns testing-gloss.core
  (:require [gloss.core :refer :all]
            [gloss.io :refer :all])
  (:import java.io.FileOutputStream))

(defmacro defpeer-wire-msg [msg-type & fields]
  (let [type-kw (keyword msg-type)]
    `(defcodec ~msg-type
       (ordered-map :type ~type-kw ~@ fields))))

(defcodec types
  (enum :ubyte :choke :unchoke :interested :not-interested :have
        :bitfield :request :piece :cancel))

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

(def peer-wire-messages
  (finite-frame :uint32 (header types {:choke choke} :type)))

(defcodec handshake
  (ordered-map :protocol-name (finite-frame :ubyte (string :us-ascii))
               :reserved      :uint64
               :info-hash     (repeat 20 :ubyte)
               :peer-id       (string :us-ascii :length 20)))

(defn sha1-to-byte-seq [sha1]
  (seq (byte-array (drop 1 (.toByteArray (biginteger sha1))))))

(with-open [out (FileOutputStream. "handshake.bin")]
  (encode-to-stream handshake
                    out
                    [{:protocol-name "BitTorrent protocol"
                      :reserved  0
                      :info-hash (sha1-to-byte-seq 0xd8a871a8485f51c2b399f78af161d0fca35b5c46)
                      :peer-id   "bt.clj  ------------"}]))

