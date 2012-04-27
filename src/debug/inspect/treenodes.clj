; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v1.0.txt at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns debug.inspect.treenodes
  {:author "Gunnar Völkel"}
  (:use [clojure.contrib.def :only (defvar-)])
  (:use swing.treetable swing.resources))



(defmulti get-treenode-icons "Gets the icon for the given type of tree node." identity)

(defmethod get-treenode-icons :default [_] nil)



(defmulti create-treenode "Creates a tree node for the given object." type)



(defmacro def-treenode-with-icons
  [type, icon-map, & fn-args]
 `(do
    (defmethod get-treenode-icons ~type [_#] ~icon-map)
    (defmethod create-treenode ~type ~@fn-args)))






