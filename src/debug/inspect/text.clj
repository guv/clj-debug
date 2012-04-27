(ns debug.inspect.text
  (:use [clojure.contrib.def :only (defvar-)])
  (:use [debug.inspect.str :only (debug-str)])  
  (:use [debug.tools :only (extract-realized-seq)]))


(defmulti data->text "Returns a (shortened) textual representation of an object." type)


(defmethod data->text :default 
  [obj]
  (let [non-lazy-obj (extract-realized-seq obj)]
	  (debug-str non-lazy-obj)))

(defmethod data->text nil [_] "nil")