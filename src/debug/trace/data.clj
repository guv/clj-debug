; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v1.0.txt at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

; TODO: 
; * general expression nodes (not separated into expression-node and macro-node)
(ns debug.trace.data
  {:author "Gunnar Völkel"})


(defprotocol ITraceNode
  (add-child! [this, child] "Adds a child node to this node.") 
  (set-result! [this, result, except?] "Sets the result of the call."))




(deftype FunctionCallNode [ns-str, name-str, param-name-value-list, sub-call-list, return]
  ITraceNode
  (add-child! [this, child]
    (swap! sub-call-list conj child)
    this)
  
  (set-result! [this, result, except?]
    (reset! return {:value result, :exception? except?})
    this))

(defn create-function-call-node 
  [ns-str, name-str]
  (FunctionCallNode.   ns-str, name-str, (atom []), (atom []), (atom nil)))

(defn add-param! [func-call-node, name, value]
  (swap! (.param-name-value-list func-call-node) conj {:name name, :value value}))




(deftype ExpressionCallNode [ns-str, name-str, expression, param-name-expression-list, param-call-list, return]
  ITraceNode
  (add-child! [this, child]
    (swap! param-call-list conj child)
    this)
  (set-result! [this, result, except?]
    (reset! return {:value result, :exception? except?})       
    this))



(defn- match-variable-params?
  [arglist, arg-count]
  (let [cnt (count arglist)]
    (if (some #(= % '&) arglist)
      (<= (dec cnt) arg-count)
      (= cnt arg-count))))


(defn- merge-and-with-param-name
  [arglist]
  (loop [arglist arglist, result (vector)]
    (if (empty? arglist)
      result
      (let [p (first arglist)]
        (if (= p '&)
          (recur (drop 2 arglist) (conj result (symbol (str "& " (second arglist))) ))
          (recur (rest arglist) (conj result p)))))))

(defn- determine-args
  [arglists arg-count]
  (let [args (first (filter #(match-variable-params? % arg-count) arglists))]
     (if (some #(= % '&) args)
       (merge-and-with-param-name args)
       args)))


(defn create-expression-call-node
  [expr-symbol, expr]
  (let [func-meta 
        (when-not (or (special-symbol? expr-symbol) (keyword? expr-symbol)) 
          (-> expr-symbol resolve meta)),
        arglists (:arglists func-meta),
        arg-count (count (rest expr)),
        args (determine-args arglists arg-count),
        name-expr-list (map (fn [name, expr] {:param-name name, :param-expr expr}) args (rest expr))]    
    (ExpressionCallNode. (str (:ns func-meta)), (if (nil? func-meta) (str expr-symbol) (:name func-meta)), expr, name-expr-list, (atom []), (atom nil))))



(deftype MacroNode [ns-str, name-str, expression, expanded-expression, param-name-expression-list, sub-call-list, return]
  ITraceNode
  (add-child! [this, child]
    (swap! sub-call-list conj child)
    this)
  (set-result! [this, result, except?]
    (reset! return {:value result, :exception? except?})       
    this))


(defn create-macro-node
  [expr-symbol, expr]
  (let [macro-meta
        ; TODO: check if that can be omitted (in theory we know that the symbol is a macro)
        (when-not (or (special-symbol? expr-symbol) (keyword? expr-symbol))
          (-> expr-symbol resolve meta)),
        arglists (:arglists macro-meta),
        arg-count (count (rest expr)),
        args (determine-args arglists arg-count),
        name-expr-list (map (fn [name, expr] {:param-name name, :param-expr expr}) args (rest expr))]
    (MacroNode. (str (:ns macro-meta)), (:name macro-meta), expr, (macroexpand-1 expr), name-expr-list, (atom []), (atom nil))))



(deftype SymbolNode [ns-str, name-str, sub-call-list, return]
  ITraceNode
  (add-child! [this, child]
    (swap! sub-call-list conj child)
    this)
  (set-result! [this, result, except?]
    (reset! return {:value result, :exception? except?})       
    this))

(defn create-symbol-node
  [full-qualified-symb]
  (SymbolNode. (namespace full-qualified-symb), (name full-qualified-symb), (atom []), (atom nil)))