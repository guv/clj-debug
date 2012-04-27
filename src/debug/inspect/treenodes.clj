(ns debug.inspect.treenodes
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






