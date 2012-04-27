; Gunnar Völkel:
; * NOT SUPPORTED (atm):
;   - variable parameter lists of traced functions
; * REMARKS:
;   - *-setup throws an Exception on reevaluation in REPL (via CCW CTRL+ALT+S; most likely also in general)
(ns debug.intercept
  (:require
    [debug.intercept.registry :as registry])
  (:use 
    [clojure.contrib.core :only(-?>)]
    [clojure.contrib.def :only (defalias, defvar-)]
    [clojure.string :only (join)])
  (:import java.io.File))


(defalias core-defn    clojure.core/defn)
(defalias core-deftype clojure.core/deftype)

(def global-interception? false)

(def function-interception-registry (registry/create-registry))
(def type-interception-registry (registry/create-registry))


(defn create-function-key
  "Create a key for the given namespace and symbol of the function that is used in the registry."
  [func-ns, func-symb]
  ; if the function symbol has a namespace, ...
  (if (namespace func-symb)
    ; ... then concatenate it with the namespace prefix via \. ...
    (str func-ns "." func-symb)
    ; ... else concatenate it the namespace via \/.
    (str func-ns "/" func-symb)))


(defn create-type-key
  "Create a key for the given namespace and symbol of the type that is used in the registry."
  [type-ns, type-symb]
  (str type-ns "." type-symb))


(defn process-defn-decl
  [func-decl]
  (let [; extract doc string (if present) and create metadata map
			  meta-map (if (string? (first func-decl)) 
			             {:doc (first func-decl)} 
			             {}),
			  ; remove doc string if present
			  func-decl (if (string? (first func-decl))
			              (rest func-decl)
			              func-decl),
			  ; add given metadata (if present) to map 
			  meta-map (if (map? (first func-decl))
			             (merge meta-map (first func-decl))
			             meta-map),
			  ; remove metadata if present
			  func-decl (if (map? (first func-decl))
			              (rest func-decl)
			              func-decl),
			  ; if only a single function body then put it into a list
			  func-decl (if (vector? (first func-decl))            
			                   (list func-decl)          
			                   func-decl)	    
			  ; add given metadata at the end (if present) to map 
			  meta-map (if (map? (last func-decl))
			             (merge meta-map (last func-decl))
			             meta-map),
			  ; remove metadata at the end if present
			  func-decl (if (map? (last func-decl))
			              (butlast func-decl)
			              func-decl)
       ]
     {:meta-map meta-map, :func-body-list func-decl}))


(defn print-function-interception-info
  [prefix, full-name, params]
  (println 
    (format "%s: (%s %s)"
      prefix
      (str full-name) 
      (str params))) 
  (flush))

(defn print-type-interception-info 
  [prefix, full-name, fields]
  (println 
    (format "%s: (deftype %s %s)"
      prefix
      (str full-name) 
      (str (vec fields)))))


(defn intercept-func-def
  [defn-command, intercept-func, func-symb, func-decl]
  (let [{:keys [meta-map, func-body-list]} (process-defn-decl func-decl),
        modified-func-bodies (map (partial intercept-func *ns* func-symb) func-body-list)]  
    `(~defn-command ~func-symb ~meta-map ~@modified-func-bodies)))


; erst raussuchen der registrierten interception funktionen und dann intercept-funcdef aufrufen
(defn defn-intercept-body
  [defn-command, func-symb, func-decl]
  (let [func-key (create-function-key *ns*, func-symb),
        {:keys [intercept-fn, info-prefix]} (registry/get-intercept-fn function-interception-registry, func-key)]
    ; if no interception function was found ...
    (if (nil? intercept-fn)
      ; ... then define function normally ...
     `(~defn-command ~func-symb ~@func-decl)      
      ; ... else apply interception function on function body before definition.
     `(do
        (print-function-interception-info   ~info-prefix, (str *ns* "/" '~func-symb), '~(list* (map first (:func-body-list (process-defn-decl func-decl)))) )           
        ~(intercept-func-def defn-command intercept-fn func-symb func-decl)))))


(defmacro defn-intercept
  [func-symb, & func-decl]
  (defn-intercept-body 'debug.intercept/core-defn, func-symb, func-decl))

(defmacro defn-intercept-
  [func-symb, & func-decl]
  (defn-intercept-body 'clojure.core/defn-, func-symb, func-decl))



(defn interception-setup
  [registry, create-key-fn, intercept-func, func-symbols, info-prefix] 
  (when (fn? intercept-func)
    (let [        
          func-keys (map (partial create-key-fn *ns*)  func-symbols)
         ]   
      (doseq [fkey func-keys]
        (registry/update-registry registry, intercept-func, info-prefix, fkey))
      ; return nil to prevent ExceptionInInitializerError when returning the map
      nil)))


(defn create-setup-macro
  [registry, create-key, macro-name, intercept-func, info-prefix]
 `(defmacro ~macro-name 
    [& func-symbols#]
    (do
      (interception-setup ~registry, ~create-key, ~intercept-func, func-symbols# ~info-prefix)
      (when (and (not global-interception?) (nil? (resolve (symbol (str *ns* "/defn")) ) ))          
       `(do              
          (ns-import-intercept-symbols 
            '~'~'[
                  [defn-intercept defn], 
                  [defn-intercept- defn-], 
                  [deftype-intercept deftype], 
                  [defrecord-intercept defrecord]                  
                 ])          
          nil)))))


(defn ns-import-intercept-symbols
  [symbol-coll]
  (doseq [[fname frename] symbol-coll]
    (ns-unmap *ns* frename)
    (.refer ^clojure.lang.Namespace *ns* frename (resolve (symbol "debug.intercept" (str fname))))))

(defn resolvable-fn?
  [fsymb]
  (-?> fsymb resolve var-get fn?))

(defmacro create-function-interception-setup-macro
  [macro-name, intercept-func, info-prefix]   
  (when (resolvable-fn? intercept-func)
    (create-setup-macro 'function-interception-registry, 'create-function-key, macro-name, intercept-func, info-prefix)))

(defmacro create-type-interception-setup-macro
  [macro-name, intercept-func, info-prefix]   
  (when (resolvable-fn? intercept-func)
    (create-setup-macro 'type-interception-registry, 'create-type-key, macro-name, intercept-func, info-prefix)))

      
(defn intercept-config-add
  [registry, intercept-func, info-prefix, func-symbols] 
  (when (fn? intercept-func)
    (doseq [fkey (map str func-symbols)]
      (registry/update-registry registry intercept-func, info-prefix, fkey))))


(defn combine-to-function-symbol
  [func-symb-seq]
  (let [ns (first func-symb-seq)]
    (map #(symbol (str ns) (str %)) (rest func-symb-seq))))

(defn combine-to-type-symbol
  [func-symb-seq]
  (let [ns (first func-symb-seq)]
    (map #(symbol (str ns "." %)) (rest func-symb-seq))))

(defn process-func-symbols
  [registry, combine-to-symbol-fn, intercept-func, info-prefix, func-symbol-seq]
  (let [{grouped-func-symbs true, single-func-symbs false} (group-by sequential? func-symbol-seq)]
    (->> (mapcat combine-to-symbol-fn grouped-func-symbs)
      (concat single-func-symbs)
      (intercept-config-add registry, intercept-func, info-prefix))))

(defn create-config-macro
  [registry, combine-to-symbol-fn, macro-name, intercept-func, info-prefix]
 `(defmacro ~macro-name 
     [& func-symbols#]
    `(process-func-symbols ~~registry, ~~combine-to-symbol-fn, ~~intercept-func, ~~info-prefix, '~func-symbols#)))

(defmacro create-function-interception-config-macro
  [macro-name, intercept-func, info-prefix]
  (when (resolvable-fn? intercept-func)
    (create-config-macro ''debug.intercept/function-interception-registry, ''debug.intercept/combine-to-function-symbol, macro-name, intercept-func, info-prefix)))

(defmacro create-type-interception-config-macro
  [macro-name, intercept-func, info-prefix]
  (when (resolvable-fn? intercept-func)
    (create-config-macro ''debug.intercept/type-interception-registry, ''debug.intercept/combine-to-type-symbol, macro-name, intercept-func, info-prefix)))

(defmacro create-funtion-interception-macros
  "Creates both function interception macros, the local setup macro and the global config macro, for the given interception function."
  [setup-macro-name, config-macro-name, intercept-func, info-prefix]
 `(do
    (create-function-interception-setup-macro ~setup-macro-name, ~intercept-func, ~info-prefix)
    (create-function-interception-config-macro ~config-macro-name, ~intercept-func, ~info-prefix)))

(defmacro create-type-interception-macros
  "Creates both type interception macros, the local setup macro and the global config macro, for the given interception function."
  [setup-macro-name, config-macro-name, intercept-func, info-prefix]
 `(do
    (create-type-interception-setup-macro ~setup-macro-name, ~intercept-func, ~info-prefix)
    (create-type-interception-config-macro ~config-macro-name, ~intercept-func, ~info-prefix)))


(defn create-datatype-fn-key
  [ns, datatype-name, func-symb]
  (str ns "." datatype-name "/" func-symb))

(defn intercept-spec-fn
  [ns, datatype-name, [fn-name, params, & body :as spec-fn]]
  (let [func-key (create-datatype-fn-key ns, datatype-name, fn-name),
        {:keys [intercept-fn, info-prefix]} (registry/get-intercept-fn function-interception-registry, func-key)]
    ; if no interception function found, ...
    (if (nil? intercept-fn)
      ; ... then return the function unchanged ...
      spec-fn
      ; ... else apply interception function on the body of the given function.
      (do
        (print-function-interception-info   info-prefix, func-key, params)
        (list* fn-name (intercept-fn (str ns "." datatype-name) fn-name (list* params body)))))))


(defn intercept-specs
  [ns, datatype-name, specs]
  (map #(if (list? %) (intercept-spec-fn ns, datatype-name, %) %) specs))

; type-interceptio-function: [ns, name, fields, specs] -> [fields, specs]
(defn def*-intercept
  [def-cmd, name, fields, specs]
  (let [type-key (create-type-key *ns*, name),
        {:keys [intercept-fn, info-prefix]} (registry/get-intercept-fn type-interception-registry, type-key),
        intercept-type? (fn? intercept-fn),
        [fields, specs] (if intercept-type? (intercept-fn *ns*, name, fields, specs) [fields, specs])]
   `(do
      ~(when intercept-type? `(print-type-interception-info ~info-prefix, ~type-key, '~fields)) 
      (~def-cmd ~name, ~fields, ~@(intercept-specs *ns*, name, specs)))))

(defmacro deftype-intercept
  [name, fields, & specs]
  (def*-intercept 'debug.intercept/core-deftype, name, fields, specs))

(defmacro defrecord-intercept
  [name, fields, & specs]
  (def*-intercept 'clojure.core/defrecord, name, fields, specs))

(defn file-exists?
  [file-name]
  (try
	  (when-let [f (File. file-name)]
	    (and (.exists f) (.isFile f)))
    (catch Exception e false)))


(defvar- interception-enabled? false "Determines whether interception of functions is enabled.")

(defn enable-intercept
  [enabled?]
  (alter-var-root #'interception-enabled? (constantly enabled?)))


(defn setup-global-interception
  [file-name, intercept-config-funcs]
  (when (file-exists? file-name)
    (doseq [func-symb intercept-config-funcs]
      (use [(symbol (namespace func-symb)) :only [(symbol (name func-symb))]]))
    (refer 'debug.intercept :only '[enable-intercept])
    (load-file file-name)
    (when interception-enabled?
      (alter-var-root #'clojure.core/defn (constantly (var-get #'defn-intercept)))
      (alter-var-root #'global-interception? (constantly true))
      (alter-var-root #'clojure.core/deftype (constantly (var-get #'deftype-intercept))))))

(defmacro intercept-global
  [file-name & intercept-config-funcs]
  (setup-global-interception (str file-name), intercept-config-funcs))

(defmacro intercept-global-runtime
  [file-name & intercept-config-funcs]
 `(setup-global-interception ~(str file-name), '~intercept-config-funcs))