; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v1.0.txt at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns debug.trace-test
  {:author "Gunnar Völkel"}
  (:use debug.trace debug.inspect))

(trace-setup c d f g h Blubb/toString multi)
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


;(defn multi
;  [x y & args]
;  (apply + x y args))