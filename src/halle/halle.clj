(ns hansa.halle
  (:refer-clojure
   :exclude [first last take take-last reductions every some]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(def ^:const double-type Double/TYPE)
(def ^:const double-array-type (Class/forName "[D"))
(def ^:const double-double-array-type (Class/forName "[[D"))
(def ^:const float-type Float/TYPE)
(def ^:const float-array-type (Class/forName "[F"))
(def ^:const float-float-array-type (Class/forName "[[F"))
(def ^:const long-type Long/TYPE)
(def ^:const long-array-type (Class/forName "[J"))
(def ^:const long-long-array-type (Class/forName "[[J"))

(def dtype
  {:double              double-type
   :double-array        double-array-type
   :double-double-array double-double-array-type
   :float               float-type
   :float-array         float-array-type
   :float-float-array   float-float-array-type
   :long                long-type
   :long-array          long-array-type
   :long-long-array     long-long-array-type})

(defprotocol SeqToPrimitive
  (seq->double-array [seq])
  (seq->long-array [seq])
  (seq->float-array [seq]))

(extend-protocol SeqToPrimitive
  java.util.Collection
  (seq->double-array [seq] (double-array seq))
  (seq->long-array   [seq] (long-array seq))
  (seq->float-array  [seq] (float-array seq)))

(extend-type (Class/forName "[D")
  SeqToPrimitive
  (seq->double-array [arr] arr))

(extend-type (Class/forName "[F")
  SeqToPrimitive
  (seq->float-array [arr] arr))

(extend-type (Class/forName "[J")
  SeqToPrimitive
  (seq->long-array [arr] arr))

(defprotocol Series
  (first [arr])
  (last [arr])
  (slice [arr start stop] [arr start])
  (take [arr n])
  (take-last [arr n])
  (reductions [arr f])
  (every [arr pred]))

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
    ^double (aget ^doubles arr (unchecked-dec-int (alength ^doubles arr))))
  (slice
    ([arr ^long start ^long stop]
     (java.util.Arrays/copyOfRange ^doubles arr start stop))
    ([arr ^long start]
     (java.util.Arrays/copyOfRange ^doubles arr start (alength ^doubles arr))))
  (take
    [arr ^long n]
    (java.util.Arrays/copyOfRange ^doubles arr 0 (Math/min (int n) (alength ^doubles arr))))
  (take-last [arr ^long n]
    (java.util.Arrays/copyOfRange
     ^doubles arr (Math/max 0 (- (alength ^doubles arr) n)) (alength ^doubles arr)))
  (reductions [arr f]
    (let [n (alength ^doubles arr)
          r (double-array n)]
      (loop [i 0 b 1.0]
        (if (< i n)
          (let [tmp (double (f b (aget ^doubles arr i)))]
            (aset r i tmp)
            (recur (unchecked-inc i) tmp))
          r))))
  (every [arr pred]
    (let [n (alength ^doubles arr)]
      (loop [i 0]
        (if (< i n)
          (if ^boolean (pred (aget ^doubles arr i))
            (recur (unchecked-inc i))
            false)
          true))))
  (asome [arr pred]
    (let [n (alength ^doubles arr)]
      (loop [i 0]
        (if (< i n)
          (if-not ^boolean (pred (aget ^doubles arr i))
            (recur (unchecked-inc i))
            true)
          false)))))

(extend-type (Class/forName "[F")
  Series
  (first [arr] (aget ^floats arr 0))
  (last [arr]
    (aget ^floats arr (unchecked-dec (alength ^floats arr))))
  (slice
    ([arr ^long start ^long stop]
     (java.util.Arrays/copyOfRange ^floats arr start stop))
    ([arr ^long start]
     (java.util.Arrays/copyOfRange ^floats arr start (alength ^floats arr))))
  (take
    [arr ^long n]
    (java.util.Arrays/copyOfRange ^floats arr 0 (Math/min (int n) (alength ^floats arr))))
  (take-last [arr ^long n]
    (java.util.Arrays/copyOfRange
     ^floats arr (Math/max 0 (- (alength ^floats arr) n)) (alength ^floats arr)))
  (reductions [arr f]
    (let [n (alength ^floats arr)
          r (float-array n)]
      (loop [i 0 b 1.0]
        (if (< i n)
          (let [tmp (float (f b (aget ^floats arr i)))]
            (aset r i tmp)
            (recur (unchecked-inc i) tmp))
          r))))
  (every [arr pred]
    (let [n (alength ^floats arr)]
      (loop [i 0]
        (if (< i n)
          (if ^boolean (pred (aget ^floats arr i))
            (recur (unchecked-inc i))
            false)
          true))))
  (asome [arr pred]
    (let [n (alength ^floats arr)]
      (loop [i 0]
        (if (< i n)
          (if-not ^boolean (pred (aget ^floats arr i))
            (recur (unchecked-inc i))
            true)
          false)))))


(extend-type (Class/forName "[J")
  Series
  (first [arr] (aget ^longs arr 0))
  (last [arr]
    (aget ^longs arr (unchecked-dec (alength ^longs arr))))
  (slice
    ([arr ^long start ^long stop]
     (java.util.Arrays/copyOfRange ^longs arr start stop))
    ([arr ^long start]
     (java.util.Arrays/copyOfRange ^longs arr start (alength ^longs arr))))
  (take
    [arr ^long n]
    (java.util.Arrays/copyOfRange ^longs arr 0 (Math/min (unchecked-int n) (alength ^longs arr))))
  (take-last [arr ^long n]
    (java.util.Arrays/copyOfRange
     ^longs arr (Math/max 0 (- (alength ^longs arr) n)) (alength ^longs arr)))
  (reductions [arr f]
    (let [n (alength ^longs arr)
          r (long-array n)]
      (loop [i 0 b 1]
        (if (< i n)
          (let [tmp (long (f b (aget ^longs arr i)))]
            (aset r i tmp)
            (recur (unchecked-inc i) tmp))
          r))))
  (every [arr pred]
    (let [n (alength ^longs arr)]
      (loop [i 0]
        (if (< i n)
          (if ^boolean (pred (aget ^longs arr i))
            (recur (unchecked-inc i))
            false)
          true))))
  (asome [arr pred]
    (let [n (alength ^longs arr)]
      (loop [i 0]
        (if (< i n)
          (if-not ^boolean (pred (aget ^longs arr i))
            (recur (unchecked-inc i))
            true)
          false)))))
