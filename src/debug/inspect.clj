; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v1.0.txt at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns debug.inspect
  "Improved graphical object inspector for Clojure data structures.
   Added support for: 
     * clojure.lang.Atom  
     * clojure.lang.Ref
   Introduced protocol Inspectable and a macro that intercepts clojure.core/deftype
   to automatically create an implementation of Inspectable in that type.
   Hence, we can inspect the attributes of a type while debugging.
  "
  {:author "Gunnar Völkel"}
  (:import
     (java.awt BorderLayout Dimension)
     (java.awt.event ActionEvent ActionListener)
     (javax.swing.tree TreeModel DefaultTreeCellRenderer)     
     (javax.swing JTree JTable JScrollPane JFrame SwingUtilities JLabel))  
  (:use 
    (debug.inspect text treenodes gui inspectable)
    debug.intercept)
  (:use swing.treetable)  
  (:use [clojure.contrib.def :only (defvar-)]))



(defvar- inspect-column-specs 
  [(swing.treetable.ColumnSpecification.   "Structure", 600, nil)  
   (swing.treetable.ColumnSpecification.            "", 180, (create-string-cell-renderer :center))])



(defn inspect
  "Creates a graphical (Swing) inspector on the supplied hierarchical data."
  ([data]
    (inspect   data, 800, 400))
  ([data, width, height]
    (try
	    (with-tree-cell-renderer-factory   create-registered-icons-tree-cell-renderer
			  (show-tree-table  
		      (force (create-treenode (unwrap-mutable-data data))), 
		      inspect-column-specs, "Improved Clojure Inspector", true, width, height))
      (catch Throwable t (println "Inspection failed with exception") (flush)))))


(defn inspect-result
  "Calls inspect on the given parameter and returns the parameter unaltered."
  [result]
  (inspect result)
  result)



(defn- create-attribute-map [fields]
  (interleave (map (fn [s] `(quote ~s)) fields) (map #(vary-meta % (constantly nil)) fields)))

(defn intercept-type-for-inspection
  [type-ns, type-name, fields, specs]
  (let [attr-map (create-attribute-map fields)]
	  [fields
	  `(~@specs
	     Inspectable
       (attribute-map [this] (hash-map ~@attr-map)))]))
 
(create-type-interception-macros inspection-setup, inspection, intercept-type-for-inspection, "INSPECT")