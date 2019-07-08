(ns ribelo.haag
  (:refer-clojure :exclude [first last take take-last reductions]))

#?(:clj (def ^:const double-array-type (Class/forName "[D")))
#?(:clj (def ^:const double-double-array-type (Class/forName "[[D")))
#?(:clj (def ^:const long-array-type (Class/forName "[J")))
#?(:clj (def ^:const long-long-array-type (Class/forName "[[J")))

(defprotocol SeqToPrimitive
  (seq->double-array [seq])
  (seq->long-array [seq]))

#?(:clj
   (extend-protocol SeqToPrimitive
     java.util.Collection
     clojure.lang.PersistentVector
     (seq->double-array ^doubles [seq] (double-array seq))
     (seq->long-array ^longs [seq] (long-array seq))))

#?(:clj
   (extend-type (Class/forName "[D")
     SeqToPrimitive
     (seq->double-array ^doubles [arr] arr)))

#?(:clj
   (extend-type (Class/forName "[J")
     SeqToPrimitive
     (seq->long-array ^longs [arr] arr)))

(defprotocol Series
  (first [arr])
  (last [arr])
  (slice [arr start stop] [arr start])
  (take [arr n])
  (take-last [arr n])
  (reductions [arr f]))

#?(:clj
   (extend-protocol Series
     java.util.Collection
     (first [coll] (clojure.core/first coll))
     (last [coll] (clojure.core/last coll))
     (slice [coll start stop] (subvec coll start stop))
     (take [coll n] (clojure.core/take n coll))
     (take-last [coll n] (clojure.core/take-last n coll))))

#?(:clj
   (extend-type (Class/forName "[D")
     Series
     (first [arr] (aget ^doubles arr 0))
     (last [arr]
       (aget ^doubles arr (dec (alength ^doubles arr))))
     (slice
       ([arr start stop]
        (java.util.Arrays/copyOfRange ^doubles arr (int start) (int stop)))
       ([arr start]
        (java.util.Arrays/copyOfRange ^doubles arr (int start) (alength ^doubles arr))))
     (take
       [arr n]
       (java.util.Arrays/copyOfRange ^doubles arr (int 0) (Math/min (int n) (alength ^doubles arr))))
     (take-last [arr n]
       (java.util.Arrays/copyOfRange
        ^doubles arr (Math/max 0 (- (alength ^doubles arr) n)) (alength ^doubles arr)))
     (reductions [arr f]
       (let [n (alength arr)
             r (double-array n)]
         (loop [i 0 b 0.0]
           (if (< i n)
             (let [^double tmp (f b (aget ^doubles  arr i))]
               (aset r i tmp)
               (recur (inc i) tmp))
             r))))))
