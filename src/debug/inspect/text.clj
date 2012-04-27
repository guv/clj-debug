; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v1.0.txt at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns debug.inspect.text
  {:author "Gunnar Völkel"}
  (:use [clojure.contrib.def :only (defvar-)])
  (:use [debug.inspect.str :only (debug-str)])  
  (:use [debug.tools :only (extract-realized-seq)]))


(defmulti data->text "Returns a (shortened) textual representation of an object." type)


(defmethod data->text :default 
  [obj]
  (let [non-lazy-obj (extract-realized-seq obj)]
	  (debug-str non-lazy-obj)))

(defmethod data->text nil [_] "nil")