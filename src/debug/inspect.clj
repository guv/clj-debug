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
  (:require
    [clojure.stacktrace :refer [print-cause-trace]]
    [swing.treetable :as tt]
    (debug.inspect
      text
      [treenodes :as tn]
      [gui :as gui]
      [inspectable :as insp])
    [debug.intercept :as icpt]))



(def ^:dynamic *max-window-count* 10)
(def ^:private window-count (ref 0))
(def ^:private first-warning (ref true))


;void windowActivated(WindowEvent e)
;void 	windowClosed(WindowEvent e)
;void 	windowClosing(WindowEvent e)
;void 	windowDeactivated(WindowEvent e)
;void 	windowDeiconified(WindowEvent e)
;void 	windowIconified(WindowEvent e)
;void 	windowOpened(WindowEvent e)


(defn- closing-window-listener
  [^JFrame frame]
  (.addWindowListener frame,
    (reify java.awt.event.WindowListener
      (windowActivated [_, _])
      (windowClosing [_, _])
      (windowDeactivated [_, _])
      (windowDeiconified [_, _])
      (windowIconified [_, _])
      (windowOpened [_, _])
      (windowClosed [_, _]
        (dosync (alter window-count dec))))))


(defn inspect
  "Creates a graphical (Swing) inspector on the supplied hierarchical data."
  ([data]
    (inspect data, nil, 800, 400))
  ([data, title]
    (inspect data, title, 800, 400))
  ([data, title, width, height]
    (dosync
      (let [wnd-cnt (ensure window-count),
            first?  (ensure first-warning)]
        (if (< wnd-cnt *max-window-count*) 
          (try
            (let [frame (gui/show-inspect-tree-table data, title, width, height)]
                (closing-window-listener frame)
                (alter window-count inc)
                (when-not first?
                  (ref-set first-warning true))
                frame)
            (catch Throwable t (println "Inspection failed with the following exception:") (print-cause-trace t) (flush)))
          (when first?
            (ref-set first-warning false)            
            (println
              (format "Maximum number of inspect windows reached (%d)! Future calls to inspect will be ignored."
                *max-window-count*))))))))


(defn inspect-result
  "Calls inspect on the given parameter and returns the parameter unaltered."
  [result]
  (inspect result)
  result)


(defmacro local-bindings
  "Produces a map of the names of local bindings to their values."
  []
  (let [symbols (mapv #(with-meta % nil) (keys &env))]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))


(defmacro inspect-locals
  ([]
    `(inspect (local-bindings)))
  ([title]
    `(inspect (local-bindings), ~title)))

(defmacro try-inspect
  {:arglists '([& body] [title & body])}
  ([& args]
    (let [title (when (string? (first args)) (first args))
          body (if title (rest args) args)]
      `(try
         ~@body
         (catch Throwable ~'ex
           (inspect-locals ~@(when title [title]))
           (throw ~'ex))))))

(defmacro print-locals
  ([]
    `(print-locals "locals"))
  ([title]
    `(do
       (println (str "\n" ~title))
       (doseq [[lb# val#] (sort-by key (local-bindings))]
         (println lb# "= " (if (.isArray (class val#)) (vec val#) val#))))))


(defn e
  "Invokes `inspect` on the last exception in the repl bound to *e."
  []
  (inspect *e))

(defn- create-attribute-map [fields]
  (interleave (map (fn [s] `(quote ~s)) fields) (map #(vary-meta % (constantly nil)) fields)))

(defn intercept-type-for-inspection
  [type-ns, type-name, fields, specs]
  (let [attr-map (create-attribute-map fields)]
	  [fields
	  `(~@specs
	     insp/Inspectable
       (attribute-map [this] (hash-map ~@attr-map)))]))
 
(icpt/create-type-interception-macros inspection-setup, inspection, intercept-type-for-inspection, "INSPECT")