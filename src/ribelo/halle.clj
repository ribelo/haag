(ns ribelo.halle
  (:refer-clojure
   :exclude [first last take take-last reductions every some]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const double-type Double/TYPE)
(def ^:const double-array-type (Class/forName "[D"))
(def ^:const double-double-array-type (Class/forName "[[D"))
(def ^:const float-type Float/TYPE)
(def ^:const float-array-type (Class/forName "[F"))
(def ^:const float-float-array-type (Class/forName "[[F"))
(def ^:const long-type Long/TYPE)
(def ^:const long-array-type (Class/forName "[J"))
(def ^:const long-long-array-type (Class/forName "[[J"))

(defn double-array? [arr] (= (type arr) double-array-type))
(defn double-double-array? [arr] (= (type arr) double-double-array-type))
(defn float-array? [arr] (= (type arr) float-array-type))
(defn float-float-array? [arr] (= (type arr) float-float-array-type))
(defn long-array? [arr] (= (type arr) long-array-type))
(defn long-long-array? [arr] (= (type arr) long-long-array-type))

(defprotocol SeqToPrimitive
  (seq->double-array [seq])
  (seq->double-double-array [seq])
  (seq->long-array [seq])
  (seq->long-long-array [seq])
  (seq->float-array [seq])
  (seq->float-float-array [seq])
  (seq->double-array-or-copy [seq])
  (seq->long-array-or-copy [seq])
  (seq->float-array-or-copy [seq]))

(extend-protocol SeqToPrimitive
  java.util.Collection
  (seq->double-array [seq]
    (double-array seq))
  (seq->double-double-array [seq]
    (into-array double-array-type (mapv seq->double-array seq)))
  (seq->long-array [seq]
    (long-array seq))
  (seq->long-long-array [seq]
    (into-array long-array-type (mapv seq->long-array seq)))
  (seq->float-array [seq]
    (float-array seq))
  (seq->float-float-array [seq]
    (into-array float-array-type (mapv seq->float-array seq)))
  (seq->double-array-or-copy [seq]
    (double-array seq))
  (seq->float-array-or-copy [seq]
    (float-array seq))
  (seq->long-array-or-copy [seq]
    (long-array seq)))

(extend-type (Class/forName "[D")
  SeqToPrimitive
  (seq->double-array [arr] arr)
  (seq->double-array-or-copy [arr]
    (java.util.Arrays/copyOfRange ^doubles arr 0 (alength ^doubles arr))))

(extend-type (Class/forName "[F")
  SeqToPrimitive
  (seq->float-array [arr] arr)
  (seq->float-array-or-copy [arr]
    (java.util.Arrays/copyOfRange ^floats arr 0 (alength ^floats arr))))

(extend-type (Class/forName "[J")
  SeqToPrimitive
  (seq->long-array [arr] arr)
  (seq->long-array-or-copy [arr]
    (java.util.Arrays/copyOfRange ^longs arr 0 (alength ^longs arr))))

(extend-type (Class/forName "[[D")
  SeqToPrimitive
  (seq->double-double-array [arr] arr))

(extend-type (Class/forName "[[F")
  SeqToPrimitive
  (seq->float-float-array [arr] arr))

(extend-type (Class/forName "[[J")
  SeqToPrimitive
  (seq->long-long-array [arr] arr))

(defprotocol Series
  (first [arr])
  (last [arr])
  (slice [arr start stop] [arr start])
  (take [arr n])
  (take-last [arr n])
  (reductions [arr f])
  (every [arr pred])
  (some [arr pred]))

(extend-protocol Series
  java.util.Collection
  (first [coll] (clojure.core/first coll))
  (last [coll] (clojure.core/last coll))
  (slice [coll start stop]
    (let [arr (seq->double-array coll)]
      (slice arr start stop)))
  (take [coll n]
    (let [arr (seq->double-array coll)]
      (take arr n)))
  (take-last [coll n]
    (let [arr (seq->double-array coll)]
      (take-last arr n)))
  (reductions [coll f]
    (let [arr (seq->double-array coll)]
      (reductions arr f)))
  (every [coll f]
    (let [arr (seq->double-array coll)]
      (every arr f)))
  (some [coll f]
    (let [arr (seq->double-array coll)]
      (some arr f))))

(extend-type (Class/forName "[D")
  Series
  (first [arr]
    (aget ^doubles arr 0))
  (last [arr]
    ^double (aget ^doubles arr (unchecked-dec-int (alength ^doubles arr))))
  (slice
    ([arr ^long start ^long stop]
     (java.util.Arrays/copyOfRange ^doubles arr start (Math/min (int stop) (alength ^doubles arr))))
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
  (some [arr pred]
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
     (java.util.Arrays/copyOfRange ^floats arr start (Math/min (int stop) (alength ^floats arr))))
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
     (java.util.Arrays/copyOfRange ^longs arr start (Math/min (int stop) (alength ^longs arr))))
    ([arr ^long start]
     (java.util.Arrays/copyOfRange ^longs arr start (alength ^longs arr))))
  (take
    [arr ^long n]
    (java.util.Arrays/copyOfRange ^longs arr 0 (Math/min (int n) (alength ^longs arr))))
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
