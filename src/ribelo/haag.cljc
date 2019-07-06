(ns ribelo.haag)

#?(:clj (def ^:const double-array-type (Class/forName "[D")))
#?(:clj (def ^:const double-double-array-type (Class/forName "[[D")))
#?(:clj (def ^:const long-array-type (Class/forName "[J")))
#?(:clj (def ^:const long-long-array-type (Class/forName "[[J")))

(defprotocol SeqToPrimitive
  (seq->double-array [seq])
  (seq->long-array [seq]))

#?(:clj
   (extend-protocol SeqToPrimitive
     clojure.lang.PersistentVector
     (seq->double-array ^doubles [seq] (double-array seq))
     (seq->long-array ^longs [seq] (long-array seq))
     clojure.lang.LazySeq
     (seq->double-array ^doubles [seq] (double-array seq))
     (seq->long-array ^longs [seq] (long-array seq))))

#?(:clj
 (extend-type (Class/forName "[D")
   SeqToPrimitive
   (seq->double-array ^doubles [^doubles arr] arr)))

#?(:clj
 (extend-type (Class/forName "[J")
   SeqToPrimitive
   (seq->long-array ^longs [^longs arr] arr)))
