; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v1.0.txt at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns debug.inspect.str
  "Contains some \"toString\" functions that are used in debug.inspect."
  {:author "Gunnar Völkel"})

(def ^{:dynamic true} *debug-str-subitem-count* 5)
(def ^{:dynamic true} *debug-str-max-atom-length* 200)


(defn- type-tag
  [x]
  (cond 
    (nil? x) :nil
    (map? x) :map
    (set? x) :set
    (list? x) :list
    (vector? x) :vector
    (.isArray (class x)) :array
    (keyword? x) :keyword
    (seq? x) :seq ; at last to catch any none handled seqs
    :else :default))



(defmulti debug-str type-tag)

(defmethod debug-str :nil [x] "nil")

(defmethod debug-str :default
  [x]
  (cond
    (nil? x) "nil"
    (float? x)   (format "%f" x)    
    (integer? x) (format "%d" x)
    (instance? java.lang.Boolean x) (if x "true" "false")
    (keyword? x) (str x)
    (symbol? x)  (str x)    
    :else (apply str (take *debug-str-max-atom-length* (str x)))))

(defmethod debug-str :map
  [x]
  (format "Map (#%d)", (count (keys x))))  

(defmethod debug-str :set
  [x]
  (format "Set (#%d)", (count x))
) 

(defmethod debug-str :seq
  [x]
  (format "Seq (#%d)", (count x)))

(defmethod debug-str :vector
  [x]  
  (format "Vector (#%d)", (count x)))

(defmethod debug-str :list
  [x]
  (format "List (#%d)", (count x)))

(defmethod debug-str :array
  [x]
  (format "Array (#%d)", (count x)))

(defmethod debug-str :keyword
  [x]
  (str x))

(def memo-debug-str (memoize debug-str))