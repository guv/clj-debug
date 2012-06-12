; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v1.0.txt at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns debug.timing
  {:author "Gunnar Völkel"}
  (:use [clojure.string :only (join)])
  (:use [debug.timing.gui :only (show-timing-tree-table)])
  (:use debug.timing.data)
  (:use debug.intercept))

(def ^{:dynamic true} *enable-timing* false)


(def timing-trees (atom (hash-map)))
(def ^{:dynamic true} *parent-node* nil)


(def ^{:dynamic true} *estimate-handler-duration* true)
(def ^{:dynamic true} *children-handler-duration* nil)



(defn- compare-values
  [sort-key data1 data2]  
  (let [; determine the values to compare
        v1 (get-attribute data1 sort-key) 
        v2 (get-attribute data2 sort-key)]
    ; perform comparison  
	  (cond 
      ; strings are compared ascending
	    (string? v1) 
	      (compare v1 v2)
      ; numbers are compared descending
	    (number? v1) 
	      (> v1 v2))))



(defn- build-total-timing-map
  [node-hashmap]
  (loop [total-timing-map (hash-map), node-list (vals node-hashmap)]
    ; if there is no node left to process ...
    (if (empty? node-list)
      ; ... then return the constructed map
      total-timing-map
      ; ... else 
      (let [tree-node (first node-list),
            node-data @(:data tree-node),
            func-sig (:func-sig node-data),
            total-data
            ; if a total data object is alreday in the map ...
            (if (contains? total-timing-map func-sig)
              ; ... then get it ...
              (get total-timing-map func-sig)
              ; ... else create a new one
              (create-timing-data   func-sig, (:func-ns node-data), (:func-name node-data), (:func-params node-data), (:func-params-count node-data) ))]
        (recur 
          ; update summary data
          (assoc total-timing-map 
            func-sig
            (merge-duration-statistic   total-data, node-data)),
          ; add children of this tree node to the next node list
          (concat (rest node-list) (-> tree-node :children deref vals)))))))


(defn build-func-sig 
  [func-ns, func-name, func-params]
  (str "(" func-ns "/" func-name " [" func-params "])"))


; printing result
(defn print-timings
  "Prints a table of the recorded timings.
   There may be some symbols behind the numbers with the following meanings:
   - ? occuring on every number: too few recorded timings to determine outliers
   - * occuring only in the #CALLS column: there was only one outlier which is probably due to compilation
   - * occuring on every number: there are outliers (number displayed in brackets in the #CALLS column) that are exluded in the displayed timings. 
  "
  ([] (print-timings :func-sig))
  ([sort-key]
    (let [sorted-data 
          (sort 
            (fn [v1 v2] (compare-values sort-key v1 v2)) 
            ;(vals @*timing-data-per-function*)
            (vals (build-total-timing-map @timing-trees))),
          max-sig-length
          (reduce
            #(max %1 (count %2))
            0
	          (map :func-sig sorted-data)),
          space-after-title "   "
          ruler-length (+ max-sig-length (count space-after-title) 13 (* 4 16) (* 4 3) 3)
          ruler (apply str (repeat ruler-length "-"))]
      (do        
        (let [method-title "Method signature"
              title-fill-space (apply str (repeat (- max-sig-length (count method-title)) " ") )]
          (println 
            method-title
            title-fill-space
            space-after-title
            (format "%13s     %16s  %16s  %16s  %16s"
              "#CALLS"
              "MIN [ms]"
              "MAX [ms]"
              "AVG [ms]"
              "SUM [ms]"))
          (println ruler))
	      (doall
	        (map
	          (fn [v] 
              (let [sig (:func-sig v),
                    fill-space (apply str (repeat (- max-sig-length (count sig)) " ") )
                    outlier-count (get-outlier-count v),
                    outlier-mark 
                    (if (skip-outlier-determination? (get-call-count v) *outlier-guess*) 
                      "?"
                      (if (< (get-outlier-count v) 2) " " "*"))
                    count-outlier-mark
                    (if (skip-outlier-determination? (get-call-count v) *outlier-guess*) 
                      "?   "
                      (if (zero? outlier-count) "    " (format "*(%d)" outlier-count)))]
			            (println
		                sig
		                fill-space
		                space-after-title                    
			              (format "%,13d%s %,16.3f%s %,16.3f%s %,16.3f%s %,16.3f%s" 	                
			                (get-call-count v) 
                      count-outlier-mark
			                (/ (get-duration-min v) 1000000.0)
                      outlier-mark
                      (/ (get-duration-max v) 1000000.0)
                      outlier-mark
                      (/ (get-duration-avg v) 1000000.0)
                      outlier-mark
	                    (/ (get-duration-sum v) 1000000.0)
                      outlier-mark))))
	          sorted-data))
        nil))))



; timing control

(defn timing+ []
  (alter-var-root #'*enable-timing* (constantly true))  
  (reset! timing-trees (hash-map)))

(defn timing- []
  (alter-var-root #'*enable-timing* (constantly false))
  (print-timings :duration-sum))

(defmacro with-timing
  [& commands]
 `(try
    (do    
		  (timing+)
		  (let [result# (do ~@commands)]
		    (timing-)        
		    result#))
    (catch Throwable t# (timing-) (throw t#))))

(defmacro with-timing*
  [& commands]
 `(binding [*estimate-handler-duration* false]
    (with-timing ~@commands)))




(defn get-or-create-node
  [node-hashmap-atom, func-sig, func-ns-str, func-name-str, param-str, param-count]
  (if (contains? @node-hashmap-atom func-sig)
    ; ... then return that node ...
    (get @node-hashmap-atom func-sig)
    ; ... else create an initial node and return it.
    (let [init-data (create-timing-data func-sig, func-ns-str, func-name-str, param-str, param-count )
          init-node (create-data-tree-node init-data)]
      ; add initial node
      (swap! node-hashmap-atom assoc func-sig init-node)
      ; return initial node
      init-node)))


(defn get-node-for-call
  [func-ns-str, func-name-str, param-str, param-count]
  (let [func-sig (build-func-sig func-ns-str, func-name-str, param-str)
        parent *parent-node*]
    ; if there is no parent node ...
    (if (nil? parent)
      ; ... then get (if exsists) or create node as root node ...
      (get-or-create-node   timing-trees, func-sig, func-ns-str, func-name-str, param-str, param-count)
      ; ... else get (if exsists) or create node as child of the parent.
      (get-or-create-node   (:children parent), func-sig, func-ns-str, func-name-str, param-str, param-count))))



(defn update-timing-data
  [func-data, duration, children-handler-duration]
  (let [real-duration (if *estimate-handler-duration* (- duration children-handler-duration) duration)]
    (update-duration-statistic func-data real-duration)))

(defmacro timing-handler
  [body, func-ns-str, func-name-str, param-str, param-count] 
  ; if timing is enabled ...
  `(if *enable-timing*
    ; ... then perform time measuring ...    
    (let [handler-start-time# (System/nanoTime)
          ; get or create node for function (as root node or as child of its current parent)
          local-func-node# (get-node-for-call   ~func-ns-str, ~func-name-str, ~param-str, ~param-count)          
          ; get parent
          this-parent-node# *parent-node*
          this-parent-handler-duration# *children-handler-duration*] 
      (binding [*parent-node* local-func-node#, *children-handler-duration* (atom 0)]
		    (let [func-sig# (build-func-sig   ~func-ns-str, ~func-name-str, ~param-str),
		          start-time# (System/nanoTime),
		          value# (try (do ~@body) (catch Throwable t# t#)),
		          duration# (- (System/nanoTime) start-time#)]                           
          ; record local data
          (swap! (:data local-func-node#) update-timing-data duration# @*children-handler-duration* )
          (when-not (nil? this-parent-node#)
	          ; note handler duration for parent
	          (let [; measure handler time (rough approximation)          
                  start-and-duration# (+ handler-start-time# duration#)
                  handler-duration# (+ (- (System/nanoTime) start-and-duration# ) @*children-handler-duration*)]             		          
		          (swap! this-parent-handler-duration# + handler-duration#)))
          ; return the result of the execution of the function body. rethrow exception if one was caught
		      (if (instance? Throwable value#)
            (throw value#)
            value#))))
    ; ... else just execute (inspect @*timing-trees*)the function.
    (do ~@body)))

; function definition interception for timing

(defn intercept-for-timing
  [func-ns, func-symb, params-and-body]
  (let [func-ns-str (str func-ns),
        func-name-str (str func-symb),
        params (first params-and-body),
        body (rest params-and-body),
        param-str (join ", " params),
        param-count (count params)]
    (list params `(timing-handler ~body, ~func-ns-str, ~func-name-str, ~param-str, ~param-count))))


(defmacro timing-block
  [name & body]
 `(timing-handler ~body (str ~*ns*) (str ~name) "" 0))


(defmacro defn-timing
  [func-symb & func-decl]    
  (intercept-func-def 'clojure.core/defn intercept-for-timing func-symb func-decl))

(defmacro defn-timing-
  [func-symb & func-decl]  
  (intercept-func-def 'clojure.core/defn- intercept-for-timing func-symb func-decl))

; create the timing setup and the timing config macro
(create-funtion-interception-macros timing-setup, timing, intercept-for-timing, "TIMING") 



(def ^{:private true} default-sort-key :duration-sum)

(defn show-timing-tree
  ([]
    (show-timing-tree   default-sort-key))
  ([sort-key]
    (when-not (empty? @timing-trees)
	    (show-timing-tree-table   @timing-trees, sort-key)))
  ([width, height]
    (show-timing-tree   default-sort-key, width, height))
  ([sort-key, width, height]
	  (when-not (empty? @timing-trees)
	    (show-timing-tree-table   @timing-trees, sort-key, width, height))))