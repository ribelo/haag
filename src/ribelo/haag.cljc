(ns ribelo.haag)

#?(:clj (def ^:const double-array-type (Class/forName "[D")))
#?(:clj (def ^:const double-double-array-type (Class/forName "[[D")))
#?(:clj (def ^:const long-array-type (Class/forName "[J")))
#?(:clj (def ^:const long-long-array-type (Class/forName "[[J")))

#?(:clj
   (defn seq->double-array
     "Convert sequence to double array."
     ^doubles [vs]
     (cond
       (nil? vs) nil
       (= (type vs) double-double-array-type) vs
       (seqable? vs) (double-array vs)
       :else (let [arr (double-array 1)]
               (aset arr 0 (double vs))
               arr))))

#?(:clj
   (defn seq->long-array
     "Convert sequence to long array."
     ^doubles [vs]
     (cond
       (nil? vs) nil
       (= (type vs) long-long-array-type) vs
       (seqable? vs) (long-array vs)
       :else (let [arr (long-array 1)]
               (aset arr 0 (long vs))
               arr))))
