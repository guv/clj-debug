(ns debug.trace-test
  (:use debug.trace debug.inspect))

(trace-setup c d f g h Blubb/toString)
(inspection-setup Blubb)

  
(defmacro square [expr] `(* ~expr ~expr))

(defn c [x] (/ x 10.0)) 
  
(def *bla* (map c (range 10)))  
  
(defn d
  ([x] (d x (* x x)))
  ([x y] (/ (- x y) (+ x y))))
  
(defn f [x y] (* (inc x) (+ y (reduce + *bla*)) ))
(defn g [a] (+ (square (f (dec a) (inc a))) (d (inc a) (dec a)) ) )
(defn h [b] (/ 1 b))


(deftype Blubb [a]
  Object
  (toString [this] 
    (str "I am Blubb! g(" a ") = " (g a))))

(defmacro silence
  [& body]
 `(try ~@body (catch Throwable t# nil)))

(with-trace (g 5) (.toString (Blubb. 10)) (silence (h 0)))
(with-trace (inspect (Blubb. 42)))