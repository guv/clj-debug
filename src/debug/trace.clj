; Gunnar Völkel:
; - NOT SUPPORTED (atm):
;   * variable parameter lists of traced functions
;
; - TODO:
;   * capture only evaluated elements of lazy-sequences in traces
;   * improve trace nodes and their gui nodes (not one defrecord per trace node type!), 
;     e.g. adjust GUI to reprsent the semantics of the clojure.core macros
;   * there is still a lot of redundancy in the multimethod implementations for handling known macros
;   * expression nodes should contain the expression form with filled in parameters
;   * special treatment for deref-data (copy value when tracing) - similar for lazy-seq (copy realized portion of the lazy-seq)

(ns debug.trace 
  "Library that enables tracing of function calls without having to change the function definitions.  
  "
  {:author "Gunnar Völkel"}  
  (:use [debug.inspect :only (inspect)])     
  (:use debug.intercept)  
  (:use debug.trace.data)
  (:use debug.trace.gui)
  (:use clojure.stacktrace))



(def ^{:dynamic true} *enable-trace* false)
(def call-tree (atom []))

(def ^{:dynamic true} *parent-call-node* nil)



(defn macro? [symb] 
  (cond
    (symbol? symb)
      (:macro (meta (resolve symb)))
    (var? symb)
      (:macro (meta symb))))

(defn macro-expr? [expr]
  (when (seq? expr)
    (let [ symb (first expr) ]
      (macro? symb))))



(defn- add-node-to-call-tree!
  [call-node]
  (if (nil? *parent-call-node*)
    (swap! call-tree conj call-node) 
    (add-child! *parent-call-node* call-node)))


(defn trace-begin-function-call  
  [func-ns-str, func-name-str]
  (let [func-node (create-function-call-node   func-ns-str, func-name-str)]
    (add-node-to-call-tree! func-node)       
    func-node))

(defn trace-end-function-call
  [result, exception?]
  (when-not (nil? *parent-call-node*)
    (if (instance? debug.trace.data.FunctionCallNode *parent-call-node*)
      (set-result! *parent-call-node* result exception?)
      (throw (Exception. "TRACING: Trying to end a FunctionCallNode but *parent-call-node* is a different type!")))))

(defn trace-begin-expr-call
  [variable, expr]
  (let [expr-node (create-expression-call-node variable expr)]
    (add-node-to-call-tree! expr-node)       
    expr-node))

(defn trace-end-expr-call
  [result, exception?]
  (when-not (nil? *parent-call-node*)
    (if (instance? debug.trace.data.ExpressionCallNode *parent-call-node*)
      (set-result! *parent-call-node* result exception?)
      (throw (Exception. "TRACING: Trying to end a ExpressionCallNode but *parent-call-node* is a different type!")))))

(defn trace-begin-macro-call
  [variable, expr]
  (let [macro-node (create-macro-node variable expr)]
    (add-node-to-call-tree! macro-node)       
    macro-node))

(defn trace-end-macro-call
  [result, exception?]
  (when-not (nil? *parent-call-node*)
    (if (instance? debug.trace.data.MacroNode *parent-call-node*)
      (set-result! *parent-call-node* result exception?)
      (throw (Exception. "TRACING: Trying to end a MacroNode but *parent-call-node* is a different type!")))))

(defn trace-begin-symbol-call
  [symb]
  (let [symb-node (create-symbol-node symb)]
    (add-node-to-call-tree! symb-node)       
    symb-node))

(defn trace-end-symbol-call
  [result, exception?]
  (when-not (nil? *parent-call-node*)
    (if (instance? debug.trace.data.SymbolNode *parent-call-node*)
      (set-result! *parent-call-node* result exception?)
      (throw (Exception. "TRACING: Trying to end a SymbolNode but *parent-call-node* is a different type!")))))


(defn handle-return
  [handler, result]
  (if (instance? Throwable result)
    (do      
      (handler result true)
      (throw result))
    (do         
      (handler result false)))
  result)

(defmacro execute
  [return-handler, expr]
 `(~handle-return ~return-handler
     ; surround with try-catch to be able to create a consistent call tree in case of an exception
    (try
      ~expr      
      ; catch a possible exception to return it to the handler
      (catch Throwable t# t#))))


(defn trace-all-exprs [expr]
  (for [clause expr]
   `(trace-expr ~clause)))

(defn create-symbol
  [ns-symb, symb]
  (symbol (name ns-symb) (name symb)))

(defn get-namespace-symbol
  [^clojure.lang.Var v]
  (ns-name (.ns v)))

(defn full-qualify-symbol
  [symb]
  ; if the symbol has a namespace, ...
  (if (namespace symb)
    ; ... then the symbol is returned unchanged ...
    symb
    ; ... else try to resolve the symbol:
    (let [v (resolve symb)]
      ; if the symbol could be resolved, ...
      (if v
        ; ... then a new symbol with the found namespace is created ...
        (create-symbol (get-namespace-symbol v) symb)
        ; ... else return the symbol unchanged. (TODO: exception?)
        symb))))

(defn print+return [x] (println (if x x "nil"))(flush) x)  

(defn dispatch-trace-macro
  [expr]
  (-> expr first full-qualify-symbol))

(defmulti trace-macro dispatch-trace-macro)


(defn resolve-first
  [expr]
  (let [symb (if (seq? expr) (first expr) expr)]
    (if (symbol? symb)
       (full-qualify-symbol symb)
       symb)))

; trace-expr is a macro to avoid the recursion pitfall
(defmacro trace-expr
  [expr]
 `(if *enable-trace*
    ~(if (seq? expr)
       (if (or (special-symbol? (first expr)) (macro-expr? expr)) 
         (trace-macro expr)         
        `(binding [*parent-call-node* (trace-begin-expr-call '~(resolve-first expr) '~expr)]
           (execute ~trace-end-expr-call
             (~(first expr) ~@(trace-all-exprs (rest expr))))))
       (if (symbol? expr)
        `(binding [*parent-call-node* (trace-begin-symbol-call '~(resolve-first expr))]
           (execute ~trace-end-symbol-call ~expr))
         ; else
         expr))
     ; ... else just return expression unchanged.
     ~expr))


(defmethod trace-macro :default
  [expr]
 `(binding [*parent-call-node* (trace-begin-macro-call '~(resolve-first expr) '~expr)] 
    (execute ~trace-end-macro-call (trace-expr ~(macroexpand-1 expr)))))

(defmethod trace-macro 'if
  [expr]
 `(binding [*parent-call-node* (trace-begin-expr-call '~(resolve-first expr) '~expr)]
    (execute ~trace-end-expr-call
      (~(first expr) ~@(trace-all-exprs (rest expr))))))

(defmethod trace-macro 'try
  [expr]
 `(binding [*parent-call-node* (trace-begin-expr-call '~(resolve-first expr) '~expr)]
    (execute ~trace-end-expr-call
	    (~(first expr) 
       ~@(for [form (rest expr)]
           (if (seq? form)
	           (case (first form)
	             'catch (concat (take 3 form) (trace-all-exprs (drop 3 form)))
	             'finally (concat (take 1 form) (trace-all-exprs (drop 1 form)))
	             `(trace-expr ~form))
             form))))))

(defmethod trace-macro 'throw
  [expr]
 `(binding [*parent-call-node* (trace-begin-expr-call '~(resolve-first expr) '~expr)]
    (execute ~trace-end-expr-call
      (~(first expr) ~@(trace-all-exprs (rest expr))))))
  
(defmethod trace-macro 'clojure.core/when
  [expr]
 `(binding [*parent-call-node* (trace-begin-expr-call '~(resolve-first expr) '~expr)]
    (execute ~trace-end-expr-call
      (~(first expr) ~@(trace-all-exprs (rest expr))))))

(defmethod trace-macro 'clojure.core/cond
  [expr]
 `(binding [*parent-call-node* (trace-begin-expr-call '~(resolve-first expr) '~expr)]
    (execute ~trace-end-expr-call
      (~(first expr) 
        ~@(for [clause (rest expr)]
		        (if (= clause :else) 
		          clause 
		         `(trace-expr ~clause)))))))



(defmethod trace-macro 'clojure.core/let
  [expr](println "let!")
 `(binding [*parent-call-node* (trace-begin-expr-call '~(resolve-first expr) '~expr)]
    (execute ~trace-end-expr-call
	    (~(first expr) 
	      [~@(apply concat
             (for [[symb form] (partition 2 (second expr))] [symb `(trace-expr ~form)]))]
         ~@(trace-all-exprs (rest (rest expr)))))))

(defmethod trace-macro 'clojure.core/if-let
  [expr]
 `(binding [*parent-call-node* (trace-begin-expr-call '~(resolve-first expr) '~expr)]
    (execute ~trace-end-expr-call
	    (~(first expr) 
	      [~(first (second expr)) (trace-expr ~(second (second expr)))]
         ~@(trace-all-exprs (rest (rest expr)))))))


(defn trace-param-expr 
  [param-expr]
 `(if (instance? debug.trace.data.FunctionCallNode *parent-call-node*)
    (add-param! *parent-call-node* (str '~param-expr) ~param-expr)
    (throw (Exception. "TRACING: *parent-call-node* is no FunctionCallNode!"))))


(defn trace-handler
  [param-list, body, func-ns-str, func-name-str]  
  ; if trace is enabled ...
 `(if *enable-trace*
    ; ... then perfom trace by
	  (do
      ; create data structure to record this function call
	    (binding [*parent-call-node* (trace-begin-function-call ~func-ns-str, ~func-name-str)]
        ; trace all given parameters
	      ~@(for [param param-list] (trace-param-expr param))
        ; handle function return value (maybe exception) of the executed traced body expression      
        (execute ~trace-end-function-call
          (do ~@(for [expr body] `(trace-expr ~expr))))))
    ; ... else just execute the body.
	  (do ~@body)))



; function definition interception for trace

(defn intercept-for-trace
  [func-ns, func-symb, params-and-body]
  (let [func-ns-str (str func-ns),
        func-name-str (str func-symb),
        param-list (first params-and-body),
        body (rest params-and-body)]    
    (list param-list (trace-handler param-list, body, func-ns-str, func-name-str))))


(defmacro defn-trace
  [func-symb & func-decl]    
  (intercept-func-def 'clojure.core/defn intercept-for-trace func-symb func-decl))

(defmacro defn-trace-
  [func-symb & func-decl]  
  (intercept-func-def 'clojure.core/defn- intercept-for-trace func-symb func-decl))

; create the tracing setup and the tracing config macro
(create-funtion-interception-macros trace-setup, trace, intercept-for-trace, "TRACE") 


(defn trace+ []
  (alter-var-root #'*enable-trace* (constantly true))
  (reset! call-tree []))

(defn trace-   
  []
  (alter-var-root #'*enable-trace* (constantly false))
  ; only show tree window when call tree is not empty  
  (when-not (empty? @call-tree)
    (inspect @call-tree)))

(defmacro with-trace
  [& commands]
 `(try
    (do    
		  (trace+)
		  (let [result# (do ~@commands)]
		    (trace-)
		    result#))
    (catch Throwable t# (trace-) (throw t#))))