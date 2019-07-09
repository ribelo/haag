(ns ribelo.haag
  (:refer-clojure
   :exclude [first last take take-last reductions])
  (:require
   [primitive-math :as p]))

(set! *warn-on-reflection* true)

(def ^:const double-array-type (Class/forName "[D"))
(def ^:const double-double-array-type (Class/forName "[[D"))
(def ^:const long-array-type (Class/forName "[J"))
(def ^:const long-long-array-type (Class/forName "[[J"))

(defprotocol SeqToPrimitive
  (seq->double-array [seq])
  (seq->long-array [seq]))

(extend-protocol SeqToPrimitive
  java.util.Collection
  clojure.lang.PersistentVector
  (seq->double-array ^doubles [seq] (double-array seq))
  (seq->long-array ^longs [seq] (long-array seq)))

(extend-type (Class/forName "[D")
  SeqToPrimitive
  (seq->double-array ^doubles [arr] arr))

(extend-type (Class/forName "[J")
  SeqToPrimitive
  (seq->long-array ^longs [arr] arr))

(defprotocol Series
  (first [arr])
  (last [arr])
  (slice [arr start stop] [arr start])
  (take [arr n])
  (take-last [arr n])
  (reductions [arr f]))

(extend-protocol Series
  java.util.Collection
  (first [coll] (clojure.core/first coll))
  (last [coll] (clojure.core/last coll))
  (slice [coll start stop] (subvec coll start stop))
  (take [coll n] (clojure.core/take n coll))
  (take-last [coll n] (clojure.core/take-last n coll)))

(extend-type (Class/forName "[D")
  Series
  (first [arr] (aget ^doubles arr 0))
  (last [arr]
    (aget ^doubles arr (p/dec (alength ^doubles arr))))
  (slice
    ([arr start stop]
     (java.util.Arrays/copyOfRange ^doubles arr (p/int start) (p/int stop)))
    ([arr start]
     (java.util.Arrays/copyOfRange ^doubles arr (p/int start) (alength ^doubles arr))))
  (take
    [arr n]
    (java.util.Arrays/copyOfRange ^doubles arr (p/int 0) (p/min (p/int n) (alength ^doubles arr))))
  (take-last [arr ^long n]
    (java.util.Arrays/copyOfRange
     ^doubles arr (p/max 0 (p/- (alength ^doubles arr) n)) (alength ^doubles arr)))
  (reductions [^doubles arr f]
    (let [n (alength ^doubles arr)
          r (double-array n)]
      (loop [i 0 b 0.0]
        (if (p/< i n)
          (let [tmp (double (f b (aget ^doubles  arr i)))]
            (aset r i tmp)
            (recur (inc i) tmp))
          r)))))
