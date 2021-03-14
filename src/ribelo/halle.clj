(ns ribelo.halle
  (:refer-clojure
   :exclude [first last take take-last reductions every some map reduce]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const double-type Double/TYPE)
(def ^:const double-array-type (Class/forName "[D"))
(def ^:const double-double-array-type (Class/forName "[[D"))
(def ^:const long-type Long/TYPE)
(def ^:const long-array-type (Class/forName "[J"))
(def ^:const long-long-array-type (Class/forName "[[J"))

(defn double-array? [arr] (= (type arr) double-array-type))
(defn double-double-array? [arr] (= (type arr) double-double-array-type))
(defn long-array? [arr] (= (type arr) long-array-type))
(defn long-long-array? [arr] (= (type arr) long-long-array-type))

(defprotocol SeqToPrimitive
  (seq->double-array [seq])
  (seq->double-double-array [seq])
  (seq->long-array [seq])
  (seq->long-long-array [seq])
  (seq->double-array-or-copy [seq])
  (seq->long-array-or-copy [seq]))

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
  (seq->double-array-or-copy [seq]
    (double-array seq))
  (seq->long-array-or-copy [seq]
    (long-array seq)))

(extend-type (Class/forName "[D")
  SeqToPrimitive
  (seq->double-array [arr] arr)
  (seq->double-array-or-copy [arr]
    (java.util.Arrays/copyOfRange ^doubles arr 0 (alength ^doubles arr))))

(extend-type (Class/forName "[J")
  SeqToPrimitive
  (seq->long-array [arr] arr)
  (seq->long-array-or-copy [arr]
    (java.util.Arrays/copyOfRange ^longs arr 0 (alength ^longs arr))))

(extend-type (Class/forName "[[D")
  SeqToPrimitive
  (seq->double-double-array [arr] arr))

(extend-type (Class/forName "[[J")
  SeqToPrimitive
  (seq->long-long-array [arr] arr))

(defprotocol Series
  (first [arr])
  (last [arr])
  (slice [arr start stop] [arr start])
  (-take [arr n])
  (-take-last [arr n])
  (-reductions [arr f])
  (-every [arr pred])
  (-some [arr pred])
  (-map
    [arr f]
    [arr1 arr2 f]
    [arr1 arr2 arr3 f]
    [arr1 arr2 arr3 arr4 f])
  (-reduce [arr f init])
  (transpose [arr2d]))

(extend-protocol Series
  java.util.Collection
  (first [coll] (clojure.core/first coll))
  (last [coll]  (clojure.core/last coll))
  (slice [coll start stop]
    (let [arr (seq->double-array coll)]
      (slice arr start stop)))
  (-take [coll n]
    (let [arr (seq->double-array coll)]
      (-take arr n)))
  (-take-last [coll n]
    (let [arr (seq->double-array coll)]
      (-take-last arr n)))
  (-reductions [coll f]
    (let [arr (seq->double-array coll)]
      (-reductions arr f)))
  (-every [coll f]
    (let [arr (seq->double-array coll)]
      (-every arr f)))
  (-some [coll f]
    (let [arr (seq->double-array coll)]
      (-some arr f)))
  (-map
    ([coll f]
     (let [arr (seq->double-array coll)]
       (-map arr f)))
    ([c1 c2 f]
     (let [a1 (seq->double-array c1)
           a2 (seq->double-array c2)]
       (-map a1 a2 f)))
    ([c1 c2 c3 f]
     (let [a1 (seq->double-array c1)
           a2 (seq->double-array c2)
           a3 (seq->double-array c3)]
       (-map a1 a2 a3 f)))
    ([c1 c2 c3 c4 f]
     (let [a1 (seq->double-array c1)
           a2 (seq->double-array c2)
           a3 (seq->double-array c3)
           a4 (seq->double-array c4)]
       (-map a1 a2 a3 a4 f))))
  (-reduce [coll f init]
    (let [arr (seq->double-array coll)]
      (-reduce arr f init)))
  (transpose [coll2d]
    (let [arr2d (seq->double-double-array coll2d)]
      (transpose arr2d))))

(defn take       [n coll]        (-take coll n       ))
(defn take-last  [n coll]        (-take-last coll n  ))
(defn reductions [f coll]        (-reductions coll f ))
(defn every      [pred coll]     (-every coll pred   ))
(defn some       [pred coll]     (-some  coll pred   ))
(defn map       ([f c]           (-map c           f ))
                ([f c1 c2]       (-map c1 c2       f ))
                ([f c1 c2 c3]    (-map c1 c2 c3    f ))
                ([f c1 c2 c3 c4] (-map c1 c2 c3 c4 f)))
(defn reduce    ([f coll]        (-reduce coll f 0.0 ))
                ([f val coll]    (-reduce coll f val)))

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
  (-take
    [arr ^long n]
    (java.util.Arrays/copyOfRange ^doubles arr 0 (Math/min (int n) (alength ^doubles arr))))
  (-take-last [arr ^long n]
    (java.util.Arrays/copyOfRange
     ^doubles arr (Math/max 0 (- (alength ^doubles arr) n)) (alength ^doubles arr)))
  (-reductions [arr f]
    (let [n (alength ^doubles arr)
          r (double-array n)]
      (loop [i 0 b 1.0]
        (if (< i n)
          (let [tmp (double (f b (aget ^doubles arr i)))]
            (aset r i tmp)
            (recur (unchecked-inc i) tmp))
          r))))
  (-every [arr pred]
    (let [n (alength ^doubles arr)]
      (loop [i 0]
        (if (< i n)
          (if ^boolean (pred (aget ^doubles arr i))
            (recur (unchecked-inc i))
            false)
          true))))
  (-some [arr pred]
    (let [n (alength ^doubles arr)]
      (loop [i 0]
        (if (< i n)
          (if-not ^boolean (pred (aget ^doubles arr i))
            (recur (unchecked-inc i))
            true)
          false))))
  (-map
    ([arr f]
     (let [n (alength ^doubles arr)
           r (double-array n)]
       (dotimes [i n]
         (aset r i ^double (f (aget ^doubles arr i))))
       r))
    ([a1 a2 f]
     (let [n (alength ^doubles arr)
           r (double-array n)]
       (dotimes [i n]
         (aset r i ^double (f (aget ^doubles a1 i)
                              (aget ^doubles a2 i))))
       r)))
  (-map
    ([arr f]
     (let [n (alength ^doubles arr)
           r (double-array n)]
       (dotimes [i n]
         (aset r i ^double (f (aget ^doubles arr i))))
       r))
    ([a1 a2 f]
     (let [n (alength ^doubles a1)
           r (double-array n)]
       (dotimes [i n]
         (aset r i ^double (f (aget ^doubles a1 i)
                              (aget ^doubles a2 i))))
       r))
    ([a1 a2 arr3 f]
     (let [n (alength ^doubles a1)
           r (double-array n)]
       (dotimes [i n]
         (aset r i ^double (f (aget ^doubles a1 i)
                              (aget ^doubles a2 i)
                              (aget ^doubles arr3 i))))
       r))
    ([a1 a2 arr3 arr4 f]
     (let [n (alength ^doubles a1)
           r (double-array n)]
       (dotimes [i n]
         (aset r i ^double (f (aget ^doubles a1 i)
                              (aget ^doubles a2 i)
                              (aget ^doubles arr3 i)
                              (aget ^doubles arr4 i))))
       r)))
  (-reduce [arr f init]
    (let [n (alength ^doubles arr)]
      (loop [i 0 r (double init)]
        (if (< i n)
          (recur (unchecked-inc-int i) (double (f r ^double (aget ^doubles arr i))))
          r)))))

(extend-type (Class/forName "[[D")
  Series
  (transpose [arr2d]
    (let [n1 (alength ^"[[D" arr2d)
          n2 (alength ^doubles (aget ^"[[D"  arr2d 0))
          ro (make-array double-array-type n2)]
      (loop [i1 0]
        (if (< i1 n2)
          (let [ri (make-array double-type n1)]
            (aset ^"[[D" ro i1 ri)
            (loop [i2 0]
              (if (< i2 n1)
                (let [arr (aget ^"[[D" arr2d i2)
                      v   (aget ^doubles arr i1)]
                  (aset ^doubles ri i2 v)
                  (recur (unchecked-inc-int i2)))))
            (recur (unchecked-inc-int i1)))
          ro)))))

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
  (-take
    [arr ^long n]
    (java.util.Arrays/copyOfRange ^longs arr 0 (Math/min (int n) (alength ^longs arr))))
  (-take-last [arr ^long n]
    (java.util.Arrays/copyOfRange
     ^longs arr (Math/max 0 (- (alength ^longs arr) n)) (alength ^longs arr)))
  (-reductions [arr f]
    (let [n (alength ^longs arr)
          r (long-array n)]
      (loop [i 0 b 1]
        (if (< i n)
          (let [tmp (long (f b (aget ^longs arr i)))]
            (aset r i tmp)
            (recur (unchecked-inc i) tmp))
          r))))
  (-every [arr pred]
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
          false))))
  (-map
    ([arr f]
     (let [n (alength ^longs arr)
           r (long-array n)]
       (dotimes [i n]
         (aset r i ^long (f (aget ^longs arr i))))
       r))
    ([a1 a2 f]
     (let [n (alength ^longs a1)
           r (long-array n)]
       (dotimes [i n]
         (aset r i ^long (f (aget ^longs a1 i)
                            (aget ^longs a2 i))))
       r))
    ([a1 a2 arr3 f]
     (let [n (alength ^longs a1)
           r (long-array n)]
       (dotimes [i n]
         (aset r i ^long (f (aget ^longs a1 i)
                            (aget ^longs a2 i)
                            (aget ^longs arr3 i))))
       r))
    ([a1 a2 arr3 arr4 f]
     (let [n (alength ^longs a1)
           r (long-array n)]
       (dotimes [i n]
         (aset r i ^long (f (aget ^longs a1 i)
                            (aget ^longs a2 i)
                            (aget ^longs arr3 i)
                            (aget ^longs arr4 i))))
       r)))
  (-reduce [arr f init]
    (let [n (alength ^longs arr)]
      (loop [i 0 r (long init)]
        (if (< i n)
          (recur (unchecked-inc-int i) (long (f r ^long (aget ^longs arr i))))
          r)))))

(extend-type (Class/forName "[[J")
  Series
  (transpose [arr2d]
    (let [n1 (alength ^"[[J" arr2d)
          n2 (alength ^longs (aget ^"[[J"  arr2d 0))
          ro (make-array long-array-type n2)]
      (loop [i1 0]
        (if (< i1 n2)
          (let [ri (make-array long-type n1)]
            (aset ^"[[J" ro i1 ri)
            (loop [i2 0]
              (if (< i2 n1)
                (let [arr (aget ^"[[J" arr2d i2)
                      v   (aget ^longs arr i1)]
                  (aset ^longs ri i2 v)
                  (recur (unchecked-inc-int i2)))))
            (recur (unchecked-inc-int i1)))
          ro)))))
