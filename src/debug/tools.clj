(ns debug.tools
  (:use [clojure.contrib.reflect :as reflect]))



(defn lazy-seq? [x]
   (or 
     (instance? clojure.lang.LazySeq x)
     (instance? clojure.lang.Cons x)))

(defn unrealized? [x]
  (and 
    (instance? clojure.lang.LazySeq x)
    (reflect/get-field clojure.lang.LazySeq "fn" x)))


(defn extract-realized-seq [x]
  (if (and (seq? x) (lazy-seq? x))    
      (if (unrealized? x)
        (vector)
        (let [xs (seq x)]
		      (loop [xs xs, realized-seq (vector)]                        
		        (if (nil? xs)
              realized-seq
              (if (unrealized? (rest xs))
                (conj realized-seq (first xs))                         
                (recur (next xs) (conj realized-seq (first xs))))))))
    x ))




(comment

(if (unrealized? sequence)
  ; then unrealized
  nil
  (when-let [xs (seq sequence)]
    (loop [xs xs]
       
      (do
         ; do something with first element: (print-one (first xs) w)
        (if (unrealized? (rest xs))
           (.write w " ...unrealized...")
            (when-let [xs (next xs)]
              (.write w sep)
              (recur xs (dec print-length))))))))

    
       (def bla (iterate #(if (< % 100) (inc %) nil) 0))
       
(def fib-seq 
  ((fn rfib [a b] 
     (lazy-seq (cons a (rfib b (+ a b)))))
   0 1))
    
)
 