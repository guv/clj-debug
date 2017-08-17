; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v1.0.txt at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns debug.timing.gui
  {:author "Gunnar Völkel"}
  (:require [debug.tools :as tools]
            [swing.super :as s])
  (:import (org.jdesktop.swingx.tree DefaultXTreeCellRenderer)
           (org.jdesktop.swingx.treetable AbstractTreeTableModel))
  (:use [clojure.string :only (blank? split)])
  (:use debug.timing.data)
  (:use swing.treetable
        swing.resources))


(def closed-timing-icon (create-image-from-resource "debug/timing/clock-white.png"))
(def open-timing-icon (create-image-from-resource "debug/timing/clock-black.png"))
(def leaf-timing-icon (create-image-from-resource "debug/timing/clock-line.png"))


(def ^{:private true} timing-column-specs 
  [(swing.treetable.ColumnSpecification.   "Recorded Methods", 348, nil)  
   (swing.treetable.ColumnSpecification.             "#CALLS",  90, (create-int-cell-renderer))
   (swing.treetable.ColumnSpecification.          "#OUTLIERS",  50, (create-string-cell-renderer :right))
   (swing.treetable.ColumnSpecification.           "MIN [ms]", 110, (create-float-cell-renderer 6))
   (swing.treetable.ColumnSpecification.           "MAX [ms]", 110, (create-float-cell-renderer 6))
   (swing.treetable.ColumnSpecification.           "AVG [ms]", 110, (create-float-cell-renderer 6))
   (swing.treetable.ColumnSpecification.           "SUM [ms]", 110, (create-float-cell-renderer 3))
   (swing.treetable.ColumnSpecification.          "Namespace", 130, (create-string-cell-renderer :center))
   (swing.treetable.ColumnSpecification.         "Parameters", 200, (create-string-cell-renderer :center))])


(defn- get-sorted-children-list
  [data-tree-node sort-key]
  (let [children-list (-> data-tree-node :children deref vals)]
    (sort 
      (fn [child1 child2]
        (>
          (-> child1 :data deref (get-attribute sort-key)) 
          (-> child2 :data deref (get-attribute sort-key))))
      children-list)))

(def get-sorted-children-list-memo (memoize get-sorted-children-list))


(deftype TimingNode [data-tree-node sort-key]  
  ITreeTableNode
  (IsLeaf [this]
    (empty? (get-sorted-children-list-memo   data-tree-node sort-key)))
  
  (GetChildCount [this]
    (count (get-sorted-children-list-memo   data-tree-node sort-key)))
  
  (GetChild [this, index]
    (let [ child-data-node (nth (get-sorted-children-list-memo   data-tree-node sort-key) index) ]
       (TimingNode. child-data-node sort-key)))
  
  (GetValueAt [this, column]
    (let [ timing-data (-> data-tree-node :data deref) ]
      (cond
        (= column 0)  (str (:func-name timing-data) " [#" (:func-params-count timing-data) "]" )
        (= column 1)  (get-call-count     timing-data)
        (= column 2)  (get-outlier-string timing-data)        
        (= column 3)  (/ (Math/floor (get-duration-min timing-data)) 1000000.0) ;(format "%,13.3f" (/ (floor (/ (get-duration-min timing-data) 1000.0)) 1000.0) )  
        (= column 4)  (/ (Math/floor (get-duration-max timing-data)) 1000000.0) ;(format "%,13.3f" (/ (ceil (/ (get-duration-max timing-data) 1000.0)) 1000.0) ) 
        (= column 5)  (/ (Math/floor (get-duration-avg timing-data)) 1000000.0) ;(format "%,13.3f" (/ (round (/ (get-duration-avg timing-data) 1000.0)) 1000.0) ) 
        (= column 6)  (/ (Math/floor (get-duration-sum timing-data)) 1000000.0) ;(format "%,13.3f" (/ (ceil (/ (get-duration-sum timing-data) 1000.0)) 1000.0) ) 
        (= column 7)  (:func-ns         timing-data)
        (= column 8)  (format "[%s]" (:func-params     timing-data))))))


(deftype TimingRootNode [root-node-list sort-key]
  ITreeTableNode
  (IsLeaf [this] false)
  
  (GetChildCount [this]
    (count root-node-list))
  
  (GetChild [this, index]
    (let [ child-data-node (nth root-node-list index) ]
       (TimingNode. child-data-node sort-key)))
  
  (GetValueAt [this, column]
    (cond 
      (= column 0) "Recorded Methods"
      :else "")))



(defn- create-timing-tree-cell-renderer
  [^AbstractTreeTableModel tree-table-model]
  (proxy [DefaultXTreeCellRenderer] []
    (getTreeCellRendererComponent [^javax.swing.JTree tree, ^Object value, ^Boolean isSelected, ^Boolean isExpanded, ^Boolean isLeaf, ^Integer row, ^Boolean hasFocus]
      (let [renderValue (.getValueAt tree-table-model value 0),
            ^DefaultXTreeCellRenderer renderer (s/proxy-super-class DefaultXTreeCellRenderer getTreeCellRendererComponent, tree, renderValue, isSelected, isExpanded, isLeaf, row, hasFocus)]
        (if isLeaf
          (.setIcon renderer leaf-timing-icon)
          (if isExpanded
            (.setIcon renderer open-timing-icon)
            (.setIcon renderer closed-timing-icon)))          
        renderer))))


(defn show-timing-tree-table
  ""
  ([root-node-list, sort-key]
    (show-timing-tree-table   root-node-list, sort-key, 1280, 300))
  ([root-node-list, sort-key, width, height]
    (let [cp (System/getProperty "java.class.path")
          cp (when (= 1 (count (split cp #":"))) cp)]
	    (with-tree-cell-renderer-factory   create-timing-tree-cell-renderer
	      (show-tree-table 
          (TimingRootNode. (vals root-node-list) sort-key), timing-column-specs, (str "Timing results in tree structure" (if (blank? cp) "" (format " [%s]" cp))), false, width, height )))))