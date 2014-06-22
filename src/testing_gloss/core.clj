(ns testing-gloss.core
  (:require [gloss.core :refer :all]
            [gloss.io :refer :all])
  (:import java.io.FileOutputStream))

(defcodec types
  (enum :ubyte :choke :unchoke :interested :not-interested :have
        :bitfield :request :piece :cancel))

(defcodec choke   
  (ordered-map :type :choke))

(defcodec unchoke 
  (ordered-map :type :unchoke))

(defcodec interested    
  (ordered-map :type :interested))

(defcodec uninterested  
  (ordered-map :type :uninterested))

(defcodec have 
  (ordered-map :type  :have
               :index :uint32))

(defcodec bitfield 
  (ordered-map :type     :bitfield 
               :bitfield (repeated :ubyte :prefix :none)))

(defcodec request
  (ordered-map :type    :request
               :index   :uint32
               :offset  :uint32
               :length  :uint32))

(defcodec piece
  (ordered-map :type    :piece
               :index   :uint32
               :offset  :uint32
               :block   (repeated :ubyte :prefix :none)))

(defcodec cancel
  (ordered-map :type    :cancel
               :index   :uint32
               :offset  :uint32
               :length  :uint32))

(def peer-wire-messages
  (finite-frame :uint32
                (header types
                        {:choke choke}
                        :type)))

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

