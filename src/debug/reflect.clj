; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v1.0.txt at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
(ns debug.reflect
  "Implementation to show the constructors, field and methods of a given object or class.
  Inspired by the `show` function from `clojure.contrib.repl-utils`."
  (:require
    [clojure.string :as string])
  (:import
    (java.lang.reflect Modifier Method Constructor)))


(defn param-str [m]
  (->> m .getParameterTypes (map #(.getSimpleName %)) (string/join ",") (format "(%s)")))

(defn static?
  [member]
  (-> member .getModifiers Modifier/isStatic))

(defn method?
  [member]
  (instance? Method member))

(defn constructor?
  [member]
  (instance? Constructor member))

(defn result-type
  [member]
  (if (method? member) (.getReturnType member) (.getType member)))

(defn member-details [m]
  (let [static? (static? m)
        method? (method? m)
        constructor? (constructor? m)
        text (if constructor?
               (str "<init>" (param-str m))
               (str
                 (when static? "static ")
                 (.getName m)
                 " : "
                 (-> m result-type .getSimpleName)
                 " "
                 (when method? (param-str m))))]
    (assoc (bean m)           
           :static? static?
           :method? method?
           :constructor? constructor?
           :text text
           :member m)))

(defn- create-pred-fn
  [x]
  (cond
    (string? x) (fn [^String s] (-> s .toLowerCase (.contains x)))
    (instance? java.util.regex.Pattern x) (fn [^String s] (re-matches x s))
    (ifn? x) x
    :else (constantly true)))

(defn reflect
  "Prints all static and instance members of x or (class x).
  Examples: (show Integer) (show []) (show String 23) (show String \"case\") (show String #\".*Case\")"
  ([x] (show x (constantly true)))  
  ([x, pred]
	  (let [pred-fn (create-pred-fn pred)
          c (if (class? x) x (class x))
	        members (->> (concat (.getFields c) (.getMethods c) (.getConstructors c)) 
	                  (map member-details)
	                  (sort-by (juxt (comp not :static?) :method? :text)))
         filtered-members (filter (comp pred-fn :name) members)]
	    (println "=== " (Modifier/toString (.getModifiers c)) c " ===")
	    (doseq [m filtered-members]          
	      (println (:text m)))
      (println (format "===  %d of %d members  ===" (count filtered-members) (count members))))))