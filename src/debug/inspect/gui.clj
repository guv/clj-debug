; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v1.0.txt at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns debug.inspect.gui
  {:author "Gunnar Völkel"}
  (:require
    [debug.inspect.str :refer [debug-str]]
    (swing
      [treetable :as tt]
      [resources :as rsc])
    (debug.inspect
      [text :refer [data->text]]
      [treenodes :as tn]
      [inspectable :as insp])
    [debug.tools :as tools]
    [swing.super :as s])
  (:import
    org.jdesktop.swingx.tree.DefaultXTreeCellRenderer
    javax.swing.JFrame
    (org.jdesktop.swingx.treetable AbstractTreeTableModel)))


(declare popup-menu-items)


(def closed-inspect-icon (rsc/create-image-from-resource "debug/inspect/inspect-white.png"))
(def open-inspect-icon (rsc/create-image-from-resource "debug/inspect/inspect-black.png"))
(def leaf-inspect-icon (rsc/create-image-from-resource "debug/inspect/inspect-line2.png"))

(def closed-exception-icon (rsc/create-image-from-resource "debug/inspect/exception-white.png"))
(def open-exception-icon (rsc/create-image-from-resource "debug/inspect/exception-black.png"))

(def leaf-stacktrace-icon (rsc/create-image-from-resource "debug/inspect/stacktrace-line.png"))

(def closed-details-icon (rsc/create-image-from-resource "debug/inspect/info-white.png"))
(def open-details-icon (rsc/create-image-from-resource "debug/inspect/info-black.png"))


(defmethod tn/get-treenode-icons ::STACKTRACE-NODE [_]
  {:leaf-icon leaf-stacktrace-icon})

(defmethod tn/get-treenode-icons ::STACKTRACE-DETAILS-NODE [_]
  {:open-icon open-details-icon, :closed-icon closed-details-icon})

(defn atom? [x]
  (not (or (coll? x) (instance? java.util.List x))))

(defn collection-tag [x]
  (cond    
    (nil? x) :nil
    (satisfies? insp/Inspectable x) :inspectable
    (instance? java.util.Map$Entry x) :entry
    (instance? java.util.Map x) :map 
    (set? x) :set
    (.isArray (class x)) :array
    :else :atom))

(defmulti is-leaf collection-tag)
(defmulti get-child (fn [parent index] (collection-tag parent)))
(defmulti get-child-count collection-tag)


(defn unwrap-mutable-data
  [x]
  (cond
    (instance? clojure.lang.Atom x) @x
    (instance? clojure.lang.Ref x) @x
    (delay? x) (force x)
    :else x))


(defmethod is-leaf :nil [node]
  true)

(defmethod get-child :nil [parent index]
  nil)

(defmethod get-child-count :nil [parent]
  0)


(defmethod is-leaf :inspectable [node]
  false)

(defmethod get-child :inspectable [parent index]
  (unwrap-mutable-data (nth (sort-by key (insp/attribute-map parent)) index) ))

(defmethod get-child-count :inspectable [parent]
  (count (insp/attribute-map parent)))

(defmethod is-leaf :default [node]
  (atom? node))

(defmethod get-child :default [parent index]
  (unwrap-mutable-data (nth parent index)))

(defmethod get-child-count :default [parent]
  (count parent))


(defmethod is-leaf :entry [e]
  (is-leaf (unwrap-mutable-data (val e))))

(defmethod get-child :entry [e index]
  (get-child (unwrap-mutable-data (val e)) index))

(defmethod get-child-count :entry [e]
  (get-child-count (unwrap-mutable-data (val e))))


(defmethod is-leaf :map [m]
  false)

(defmethod get-child :map [m index]  
  (unwrap-mutable-data (nth (seq m) index)))

(defmethod get-child-count :map [parent]  
  (count parent))


(defmethod is-leaf :set [s]
  false)

(defmethod get-child :set [s index]  
  (unwrap-mutable-data (nth (seq s) index)))

(defmethod get-child-count :set [parent]  
  (count parent))


(defmethod is-leaf :array [node]
  false)

(defmethod get-child :array [parent index]
  (nth parent index))

(defmethod get-child-count :array [parent]
  (count parent))




(defn- debug-str-object
  [obj]  		   
  (cond
    (nil? obj) "nil"
    (float? obj)   (format "%f" obj)
    (integer? obj) (format "%d" obj)
    (instance? java.lang.Boolean obj) (if obj "true" "false")    
    (keyword? obj) (str obj)
    (symbol? obj)  (str obj)
    (string? obj)  (debug-str obj) ; debug-str will truncate the string if needed    
    :else (str obj)))

(defn- debug-str-map-key
  [k]
  (if (keyword? k)
    (str k)
    (data->text k)))
 
(defn- get-text 
  [obj]
  (-> obj unwrap-mutable-data data->text))


(defn- safe-compare [x y] 
  (try 
    (compare x y) 
    (catch ClassCastException _ 
      (compare 
        (.getName (class x)) 
        (.getName (class y))))))


(defn- get-children-seq  
  "Determine the children of the given object.   
   In case the object is a map then return the child in a sorted by key.
  "
  [obj, child-count]
  (let [children-seq (map #(get-child obj %) (range child-count))]
    ; if the object is a map, ...
		(if (instance? java.util.Map obj)
      ; ... then return the children sorted by their keys ...
		  (sort (fn [me1 me2] (safe-compare (key me1) (key me2))) children-seq)
		  ; ... else just return the children.
		  children-seq)))

  
(defmethod tn/create-treenode :default
  [obj]
  (let [realized-obj (tools/extract-realized-seq obj)
        leaf? (is-leaf realized-obj)
        child-count (if leaf? 0 (get-child-count realized-obj)),
        ; build list of delayed child node creations (only created when needed and only once)
        delayed-child-nodes
        (map 
          (fn [child-obj] (delay (tn/create-treenode child-obj)))
          ; for all child objects
          (get-children-seq realized-obj, child-count))]
    (reify
      tt/ITreeTableNode
      (IsLeaf [this]
		    leaf?)
		  (GetChildCount [this]
		    child-count)
		  (GetChild [this, index]
		    (force (nth delayed-child-nodes index)))
		  (GetValueAt [this, column]
        (if (instance? java.util.Map$Entry realized-obj)
			    (case (int column)
            0 (debug-str-map-key (key realized-obj))
				    1 (get-text (val realized-obj)) 
				    "")
          (case (int column)
            0 (get-text realized-obj)
            "")))
      (GetNodeType [this] ::INSPECTION-NODE)
      tt/IInteractive
      (context-menu-actions [this]
        (popup-menu-items this, realized-obj)))))

(defmethod tn/get-treenode-icons ::INSPECTION-NODE [_]
  {:open-icon open-inspect-icon :closed-icon closed-inspect-icon :leaf-icon leaf-inspect-icon})




(defn create-value-node
  ([value-list]
    (create-value-node   value-list, nil))
  ([value-list, node-type]
		(reify
	    tt/ITreeTableNode
	    (IsLeaf [this] true)
		  (GetChildCount [this] 0)
		  (GetChild [this, index] nil)
		  (GetValueAt [this, column]
		    (if (and (<= 0 column) (< column (count value-list)))
		      (nth value-list column)
		      ""))
	    (GetNodeType [this] node-type)
      tt/IInteractive
      (context-menu-actions [this]
        (popup-menu-items this, value-list)))))


(defn create-object-node
  ([value-list, object]
    (create-object-node   value-list, object, nil))
  ([value-list, object, node-type]
	  (let [object-node (delay (tn/create-treenode object))]
		  (reify
		    tt/ITreeTableNode
			  (IsLeaf [this] false)
			  (GetChildCount [this] 1)
			  (GetChild [this, index]
		      (when (zero? index)
	          (force object-node)))
			  (GetValueAt [this, column]
			    (if (and (<= 0 column) (< column (count value-list)))
			      (nth value-list column)
			      ""))
	      (GetNodeType [this] node-type)
       tt/IInteractive
       (context-menu-actions [this]
         (popup-menu-items this, value-list))))))


(defn create-object-sequence-node
  ([value-list, object-list]
    (create-object-sequence-node   value-list, object-list, nil))
  ([value-list, object-list, node-type]
	  (let [child-count (count object-list),	               
	        ; build list of delayed child node creations (only created when needed and only once)
	        delayed-child-nodes (map (fn [child-obj] (delay (tn/create-treenode child-obj))) object-list)]    
			(reify
			  tt/ITreeTableNode
			  (IsLeaf [this] false)
			  (GetChildCount [this] child-count)
			  (GetChild [this, index]
			    (force (nth delayed-child-nodes index)))
			  (GetValueAt [this, column]
			    (if (and (<= 0 column) (< column (count value-list)))
			      (nth value-list column)
			      ""))
	      (GetNodeType [this] node-type)
       tt/IInteractive
       (context-menu-actions [this]
        (popup-menu-items this, value-list))))))


(defn create-node-sequence-node
  ([value-list, child-node-list]
    (create-node-sequence-node  value-list, child-node-list, nil))
  ([value-list, child-node-list, node-type]
    (reify
	    tt/ITreeTableNode
      (IsLeaf [this] false)
      (GetChildCount [this] 
        (count child-node-list))
      (GetChild [this, index]
        (force (nth child-node-list index)))
      (GetValueAt [this, column]
	      (if (and (<= 0 column) (< column (count value-list)))
		       (nth value-list column)
		       ""))
      (GetNodeType [this] node-type)
      tt/IInteractive
      (context-menu-actions [this]
        (popup-menu-items this, value-list)))))



(defn create-registered-icons-tree-cell-renderer
  [^AbstractTreeTableModel tree-table-model]
  (proxy [DefaultXTreeCellRenderer] []
    (getTreeCellRendererComponent [^javax.swing.JTree tree, ^Object value, ^Boolean isSelected, ^Boolean isExpanded, ^Boolean isLeaf, ^Integer row, ^Boolean hasFocus]
      (let [renderValue (.getValueAt tree-table-model value 0),
            ^DefaultXTreeCellRenderer renderer (s/proxy-super-class DefaultXTreeCellRenderer, getTreeCellRendererComponent, tree, renderValue, isSelected, isExpanded, isLeaf, row, hasFocus),
            custom-icons (tn/get-treenode-icons (tt/GetNodeType value))]        
        (when-not (nil? custom-icons)
          (let [{:keys [open-icon closed-icon leaf-icon]} custom-icons]
            (if isLeaf
              (when-not (nil? leaf-icon) (.setIcon renderer leaf-icon))
              (if isExpanded
                (when-not (nil?   open-icon) (.setIcon renderer   open-icon))
                (when-not (nil? closed-icon) (.setIcon renderer closed-icon)))))) 
        renderer))))



(defmethod tn/create-treenode java.lang.Throwable
  [^Exception exception]
  (let [stack-trace (.getStackTrace exception),
        child-count (count stack-trace),
        leaf? (zero? child-count),
        ; build list of delayed child node creations (only created when needed and only once)
        delayed-detail-stacktrace-element-nodes
        (map 
          (fn [^StackTraceElement ste]
            (delay (create-value-node [(str (.getClassName ste) "." (.getMethodName ste)), (str (.getFileName ste) ":" (.getLineNumber ste))] ::STACKTRACE-NODE)))
          ; for all child objects
          stack-trace),
        delayed-stacktrace-element-nodes
        (map 
          (fn [^StackTraceElement ste]
            (delay (create-value-node [(str (.getClassName ste) "." (.getMethodName ste)), (str (.getFileName ste) ":" (.getLineNumber ste))] ::STACKTRACE-NODE)))
          ; for all child objects
          (filter
            (fn [^StackTraceElement ste]
              (not (some->> ste .getFileName (re-matches #".*\.java"))))
            stack-trace)),
        child-nodes
        (concat 
          delayed-stacktrace-element-nodes
          [(create-node-sequence-node ["Details"], delayed-detail-stacktrace-element-nodes, ::STACKTRACE-DETAILS-NODE)])
        cause (.getCause exception),
        child-nodes
        (if (nil? cause)
          child-nodes
          (cons 
            (tn/create-treenode cause)
            child-nodes))]
    ; stacktrace
    (create-node-sequence-node 
      [ (str (type exception)), (.getMessage exception) ]
      child-nodes
      :EXCEPTION-NODE)))

(defmethod tn/get-treenode-icons :EXCEPTION-NODE [_]
  {:open-icon open-exception-icon :closed-icon closed-exception-icon})



(def ^{:private true} inspect-column-specs 
  [(swing.treetable.ColumnSpecification.   "Structure", 600, nil)  
   (swing.treetable.ColumnSpecification.            "", 180, (tt/create-string-cell-renderer :center))])

(defn show-inspect-tree-table
  ([data]
    (show-inspect-tree-table data, nil, 800, 400))
  ([data, title]
    (show-inspect-tree-table data, title, 800, 400))
  ([data, title, width, height]  
    (tt/with-tree-cell-renderer-factory create-registered-icons-tree-cell-renderer
      (tt/show-tree-table  
        (force (tn/create-treenode (unwrap-mutable-data data))), 
        inspect-column-specs, (or title "Improved Clojure Inspector"), true, width, height))))


(defn show-inspect-tree-table-for-node
  [tree-node, ^JFrame table-frame, tree-table, e]
  (let [width (.getWidth table-frame),
        height (.getHeight table-frame),
        title (.getTitle table-frame)]
    (tt/with-tree-cell-renderer-factory create-registered-icons-tree-cell-renderer
       (tt/show-tree-table  
         tree-node, 
         inspect-column-specs,
         (format "%s - %s" title (tt/GetValueAt tree-node, 0))
         true, width, height))))


(defn popup-menu-items
  [tree-node, data]
  [["Open in separate window" (partial show-inspect-tree-table-for-node tree-node)]])