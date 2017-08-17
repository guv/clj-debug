; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v1.0.txt at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns debug.trace.gui
  {:author "Gunnar Völkel"}
  (:use [clojure.string :only (join)])
  (:use debug.trace.data)
  (:use swing.treetable
        swing.resources)
  (:use
    (debug.inspect
      treenodes
      gui
      (text :only [data->text])))
  (:import (debug.trace.data SymbolNode MacroNode ExpressionCallNode FunctionCallNode)))


(def closed-function-icon   (create-image-from-resource "debug/trace/function-white.png"))
(def open-function-icon     (create-image-from-resource "debug/trace/function-black.png"))
(def closed-result-icon     (create-image-from-resource "debug/trace/result-white.png"))
(def open-result-icon       (create-image-from-resource "debug/trace/result-black.png"))
(def closed-expression-icon (create-image-from-resource "debug/trace/expression-white.png"))
(def open-expression-icon   (create-image-from-resource "debug/trace/expression-black.png"))
(def closed-macro-icon      (create-image-from-resource "debug/trace/macro-white.png"))
(def open-macro-icon        (create-image-from-resource "debug/trace/macro-black.png"))
(def closed-symbol-icon     (create-image-from-resource "debug/trace/symbol-white.png"))
(def open-symbol-icon       (create-image-from-resource "debug/trace/symbol-black.png"))
(def closed-params-icon     (create-image-from-resource "debug/trace/params-white.png"))
(def open-params-icon       (create-image-from-resource "debug/trace/params-black.png"))
(def closed-parameter-icon  (create-image-from-resource "debug/trace/parameter-white.png"))
(def open-parameter-icon    (create-image-from-resource "debug/trace/parameter-black.png"))
(def leaf-name-icon         (create-image-from-resource "debug/trace/name-line.png"))



(defmethod get-treenode-icons ::PARAMETER-LIST-NODE [_]
  {:open-icon open-params-icon :closed-icon closed-params-icon})

(defmethod get-treenode-icons ::PARAMETER-NODE [_]
  {:open-icon open-parameter-icon :closed-icon closed-parameter-icon})

(defmethod get-treenode-icons ::NAME-NODE [_]
  {:leaf-icon leaf-name-icon})


(defmacro delayed-vector
  [& expr-list]
 `(vector ~@(map #(list 'delay %) expr-list)))


(defn- create-parameters-node
  [param-name-value-list]
  (let [parameter-nodes 
        (map 
          #(delay (create-object-node [(:name %),  (data->text (:value %))] (:value %) ::PARAMETER-NODE))
          param-name-value-list)]
    (create-node-sequence-node
      ["Parameters", (str "#" (count param-name-value-list))]
      parameter-nodes
      ::PARAMETER-LIST-NODE)))


(defn- create-sub-call-nodes
  [sub-call-list]  
  (map #(delay (create-treenode %)) sub-call-list))

(defn- create-result-node
  [result]      
  (create-object-node     
    ["Result", 
     (if (:exception? result) 
       (str (type (:value result)))
       (data->text (:value result)))],
    (:value result),
    ::RESULT-NODE))

(defmethod get-treenode-icons ::RESULT-NODE [_]
  {:open-icon open-result-icon :closed-icon closed-result-icon})



(def-treenode-with-icons FunctionCallNode, {:open-icon open-function-icon :closed-icon closed-function-icon},
  [^FunctionCallNode func-call-node]
  (let [caption (str (.name-str func-call-node) " [" (join ", " (map :name @(.param-name-value-list func-call-node)) ) "]")
        result-str (-> func-call-node (.return) deref :value data->text)]         
	  (create-node-sequence-node 
      [ caption, result-str ],  
      (concat
	      (delayed-vector
	        (create-value-node [(str (.ns-str func-call-node), "/", caption)] ::NAME-NODE)
	        (create-parameters-node @(.param-name-value-list func-call-node)))
        (create-sub-call-nodes @(.sub-call-list func-call-node))
	      (delayed-vector 
	        (create-result-node @(.return func-call-node)))),
      debug.trace.data.FunctionCallNode)))



(defn- create-parameter-info-node-list-node
  [param-name-expr-pair]
  (let [paraminfo-list
        (map 
          (fn [pair] 
            (delay 
              (create-object-node
                [(:param-name pair)]
                (str (:param-expr pair))
                ::PARAMETER-NODE)))
          param-name-expr-pair)]
    (create-node-sequence-node 
      [ "Parameter-Info", (str "#" (count param-name-expr-pair)) ]
      paraminfo-list
      ::PARAMETER-LIST-NODE)))


(def-treenode-with-icons ExpressionCallNode, {:open-icon open-expression-icon :closed-icon closed-expression-icon},
  [^ExpressionCallNode expr-call-node]
  (let [params-str (join " " (map :param-name (.param-name-expression-list expr-call-node)) ),
        caption (str (.expression expr-call-node)),
        full-spec
        (cond
          (nil? (.ns-str expr-call-node))
            (str "(" (.name-str expr-call-node), " ", params-str ")")
          (keyword? (first (.expression expr-call-node))) 
            (.name-str expr-call-node)
          (special-symbol? (first (.expression expr-call-node)))
            (.name-str expr-call-node)
          :default
            (str "(" (.ns-str expr-call-node), "/", (.name-str expr-call-node), " ", params-str ")")),
        expr-type
        (cond          
          (keyword? (first (.expression expr-call-node))) 
            "keyword"
          (special-symbol? (first (.expression expr-call-node)))
            "special symbol"
          :default
            ""),
        result-str (-> expr-call-node (.return) deref :value data->text)]
	  (create-node-sequence-node 
      [ caption, result-str ],  
      (concat
	      (delayed-vector
	        (create-value-node   [full-spec, expr-type] ::NAME-NODE)
	        (create-parameter-info-node-list-node   (.param-name-expression-list expr-call-node)))
        (create-sub-call-nodes @(.param-call-list expr-call-node))
        (delayed-vector
          (create-result-node   @(.return expr-call-node)))),
      debug.trace.data.ExpressionCallNode)))


(defn- create-expanded-expression-node
  [^MacroNode macro-node]
  (let [result-str (-> macro-node (.return) deref :value data->text),
        expanded-expr (str (.expanded-expression macro-node)),
        subcall-node-list
        (map #(delay (create-treenode %)) @(.sub-call-list macro-node))]
    (create-node-sequence-node 
      [ expanded-expr, result-str ]
      subcall-node-list)))



(def-treenode-with-icons MacroNode, {:open-icon open-macro-icon :closed-icon closed-macro-icon},
  [^MacroNode macro-node]
  (let [params-str (join " " (map :param-name (.param-name-expression-list macro-node)) ),
        caption (str (.expression macro-node)),
        full-spec (str "(" (.ns-str macro-node), "/", (.name-str macro-node), " ", params-str ")"),
        result-str (-> macro-node (.return) deref :value data->text)]     
	  (create-node-sequence-node 
      [ caption, result-str ],   
      (concat
	      (delayed-vector
	        (create-value-node   [full-spec] ::NAME-NODE)
	        (create-parameter-info-node-list-node   (.param-name-expression-list macro-node)))
        (create-sub-call-nodes   @(.sub-call-list macro-node))
        (delayed-vector
          (create-result-node   @(.return macro-node)))),
      debug.trace.data.MacroNode)))



(def-treenode-with-icons SymbolNode, {:open-icon open-symbol-icon :closed-icon closed-symbol-icon},
  [^SymbolNode symbol-node]
  (let [caption (str (.name-str symbol-node)),
        full-spec
        (if (.ns-str symbol-node) 
          (str (.ns-str symbol-node), "/", (.name-str symbol-node)),
          (str (.name-str symbol-node)))
        result-str (-> symbol-node (.return) deref :value data->text)]
	  (create-node-sequence-node 
      [ caption, result-str ], 
      (concat      
	      (delayed-vector
	        (create-value-node   [full-spec] ::NAME-NODE))
        (create-sub-call-nodes @(.sub-call-list symbol-node))
        (delayed-vector
          (create-result-node   @(.return symbol-node)))),
      debug.trace.data.SymbolNode)))