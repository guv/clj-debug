; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v1.0.txt at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns debug.intercept.registry
  {:author "Gunnar Völkel"}
  (:require [clojure.string :as string]))


(defn create-registry
  "Create a registry for interception information (function and type interception)."
  []
  (atom (hash-map)))


(defn update-registry
  "Update the interception registry by adding another interception function for the given key.
  The info-prefix is used in the interception notification. 
  "
  [registry, intercept-func, info-prefix, key]
  ; alter map by updating the function stored under the given key
  (swap! registry update-in [key]
    (fn [prev-intercept-data]
      ; if previously there was no function ...
      (if (nil? prev-intercept-data)
        ; ... then return the given function ...
        {:func-list [intercept-func], :info-prefix-strings [info-prefix]}
        ; ... else check if the intercept function is already listed, ...
        (if (some #(= (type %) (type intercept-func)) (:func-list prev-intercept-data))
          ; ... then return the previous data unchanged ...
          prev-intercept-data
          ; .. else add the function to the list
	        { 
	         :func-list 
           (conj (:func-list prev-intercept-data) intercept-func), 
	         :info-prefix-strings
	         (conj (:info-prefix-strings prev-intercept-data) info-prefix)
	        })))))


(defn- comp-partial
  "Creates a composition for the two given functions where the first two parameters are passed to both functions
  as the first two parameters. The composition is only applied to the third parameter.
  "
  [intercept-composition, intercept-fn]    
  (fn [func-ns, func-symb, params-and-body]    
    (intercept-fn func-ns, func-symb
      (intercept-composition func-ns, func-symb, params-and-body))))


(defn get-intercept-fn
  "Get the interception function for the given key from the registry."
  [registry, key]
  (let [intercept-data (get @registry key)]
    ; when interception data was found ...
    (when intercept-data
      ""
      (let [intercept-fn-list (:func-list intercept-data),
		        intercept-fn (if (< 1 (count intercept-fn-list)) (reduce comp-partial intercept-fn-list) (first intercept-fn-list)),
		        info-prefix (string/join "∘" (reverse (:info-prefix-strings intercept-data)))]
		    {:intercept-fn intercept-fn, :info-prefix info-prefix}))))