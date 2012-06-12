; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v1.0.txt at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns debug.timing.data
  {:author "Gunnar Völkel"})


(def ^{:dynamic true} *outlier-guess* 2)
(def ^{:dynamic true} *max-deviation-factor* 5.0)



(defprotocol ITimingData
  (get-call-count [this] "Returns the number of calls to the function of this data object.")
  (get-duration-min [this] "Returns the minimal execution duration of the function of this data object.")
  (get-duration-max [this] "Returns the maximal execution duration of the function of this data object.")
  (get-duration-avg [this] "Returns the average execution duration of the function of this data object.")
  (get-duration-sum [this] "Returns the total summed up execution duration of the function of this data object.")
  (get-attribute [this, keyname] "Returns the specified attribute value of this data object.")
  (get-outlier-count [this] "Return the number of outliers in the recorded values.")
  (get-outlier-string [this] "Returns a string with the outlier count if possible or \"?\" otherwise."))


(defn- average
  [value-list]
  (loop [cnt 0, avg 0.0, values value-list]
	  (if (nil? values)
	    nil
	    (if (empty? values)
	      ; return average
	      avg
	      ; increase count, update average and call calculation for next value
	      (recur (inc cnt), (/  (+ (* cnt avg) (first values))  (inc cnt)  ), (rest values))))))


(defn- sqr [x] (* x x))


(defn skip-outlier-determination?
  [value-count, outlier-guess]
  (< value-count (* 3 outlier-guess)))


(defn determine-outlier
  [poss-outlier-list, value-count, avg, sqr-avg, outlier-guess, max-deviation-factor]  
  (if (skip-outlier-determination?   value-count, outlier-guess)
    ; then
    {:values poss-outlier-list
     :outlier (list)}
    ; else
	  (let [variance (- sqr-avg (sqr avg))
	        std-dev (Math/sqrt variance)  
	        limit (* max-deviation-factor std-dev)]
	    {:values  (filter #(<= (- % avg) limit) poss-outlier-list)
	     :outlier (filter #(>  (- % avg) limit) poss-outlier-list)})))

(def determine-outlier-memo (memoize determine-outlier))



(defn separate-outliers
  [timing-data]  
  (determine-outlier-memo   
    (:poss-outlier-list timing-data), (get-call-count timing-data), (:duration-avg timing-data), 
    (:square-duration-avg timing-data), *outlier-guess*, *max-deviation-factor*))



(defrecord TimingData [func-sig func-ns func-name func-params func-params-count call-count duration-min duration-max duration-avg square-duration-avg poss-outlier-list]
  ITimingData
  (get-call-count [this]    
    (+ call-count (count poss-outlier-list) ))
  
  (get-duration-min [this]
    (let [values (-> this separate-outliers :values)]
      (if (empty? values)
        duration-min
        (let [values-min (reduce min values)]
          (if (nil? duration-min)
            values-min
            (min duration-min values-min))))))
  
  (get-duration-max [this]
    (let [ values (-> this separate-outliers :values) ]
      (if (empty? values)
        duration-max
        (let [values-max (reduce max values)]
          (if (nil? duration-max)
            values-max
            (max duration-max values-max))))))
  
  (get-duration-avg [this]
    (let [values (-> this separate-outliers :values)
          value-count (count values)]
      (if (empty? values)
        duration-avg
        (/
          (+ 
	          (* duration-avg call-count) 
	          (* (average values) value-count))
          (+ call-count value-count)))))
  
  (get-duration-sum [this]
    (* (get-duration-avg this) (- (get-call-count this) (get-outlier-count this) ) ))
  
  (get-attribute [this, keyname]
    (cond 
      (#{:func-sig :func-ns :func-name :func-params} keyname)
        (get this keyname)
      (= :call-count keyname)
        (get-call-count this)
      (= :duration-min keyname)
        (get-duration-min this)
      (= :duration-max keyname)
        (get-duration-max this)
      (= :duration-avg keyname)
        (get-duration-avg this)
      (= :duration-sum keyname)
        (get-duration-sum this)))
  
  (get-outlier-count [this]
    (count (:outlier (separate-outliers this))))
  
  (get-outlier-string [this]
    (if (skip-outlier-determination? (get-call-count this) *outlier-guess*)
      (format "%10s" "?")      
      (format "%10d" (get-outlier-count this)))))


(defn create-timing-data
  [func-sig, func-ns-str, func-name-str, func-params-str, func-params-count]
  (TimingData. func-sig, func-ns-str, func-name-str, func-params-str, func-params-count, 0, nil, nil, 0.0, 0.0, (list)))


(defn- min*
  [v1 v2]
  (if (nil? v1)
    ; v1 = nil
    v2
    ; v1 != nil
    (if (nil? v2) v1  (min v1 v2))))

(defn- max*
  [v1 v2]
  (if (nil? v1)
    ; v1 = nil
    v2
    ; v1 != nil
    (if (nil? v2) v1  (max v1 v2))))


(defn- update-avg
  [avg cnt duration]
  (/ (+ (* avg cnt) duration) (inc cnt)))

(defn update-duration-statistic
  [func-data, duration]
  ; if possible outlier list is not full, ...
  (if (< (count (:poss-outlier-list func-data))  *outlier-guess*)
    ; ... then add the current duration value ...
    (update-in func-data
      [:poss-outlier-list]
      (fn [outlier-list]
        (sort > (conj outlier-list duration))))
    ; ... else check if that value belongs in the outlier list ...
    (let [comb-sorted-list (sort > (conj (:poss-outlier-list func-data) duration)) 
          new-outlier-list (take *outlier-guess* comb-sorted-list)
          value (last comb-sorted-list)]
      (-> func-data        
        (assoc :poss-outlier-list new-outlier-list)
        (update-in [:duration-avg] update-avg (:call-count func-data) value )
        (update-in [:square-duration-avg] update-avg (:call-count func-data) (sqr value) )
        (update-in [:duration-min] 
          (fn [current-min] 
            (if (nil? current-min) 
              value 
              (min current-min value))))
        (update-in [:duration-max] 
          (fn [current-max] 
            (if (nil? current-max) 
              value 
              (max current-max value))))
        (update-in [:call-count] inc)))))

; func-sig func-ns func-name func-params call-count duration-min duration-max duration-avg square-duration-avg poss-outlier-list
(defn- merge-averages
  [avg1, cnt1, avg2, cnt2, value-list]
  (let [value-cnt (count value-list),
        value-avg (average value-list),
        total-cnt (double (+ cnt1 cnt2 value-cnt))]
    (if (zero? total-cnt)
      0.0
	    (+
	      (* (/       cnt1 total-cnt)       avg1)
	      (* (/       cnt2 total-cnt)       avg2)
	      (* (/ value-cnt  total-cnt) value-avg )))))

(defn merge-duration-statistic
  [func-data-summary, node-func-data]  
  (let [; merge possible outlier lists and sort them
        merged-poss-outlier-list 
        (sort >
          (concat 
            (:poss-outlier-list func-data-summary) 
            (:poss-outlier-list node-func-data))), 
        ; extract possible outliers
        new-poss-outlier-list (take *outlier-guess* merged-poss-outlier-list),
        ; extract non-outliers
        non-outliers (drop *outlier-guess* merged-poss-outlier-list)]
    ; via assoc we can skip the string-data here that remains equal
    (-> func-data-summary      
      (assoc :poss-outlier-list new-poss-outlier-list )
      (assoc :duration-avg 
        (merge-averages 
          (:duration-avg func-data-summary) (:call-count func-data-summary)  
          (:duration-avg node-func-data)    (:call-count node-func-data)
          non-outliers))
      (assoc :square-duration-avg        
        (merge-averages 
          (:square-duration-avg func-data-summary) (:call-count func-data-summary)  
          (:square-duration-avg node-func-data)    (:call-count node-func-data)
          (map sqr non-outliers)))
      (assoc :duration-min          
        (reduce min* nil 
          (conj non-outliers (:duration-min func-data-summary) (:duration-min node-func-data))))
      (assoc :duration-max          
        (reduce max* nil 
          (conj non-outliers (:duration-max func-data-summary) (:duration-max node-func-data))))       
      (assoc :call-count 
        (+ (:call-count func-data-summary) (:call-count node-func-data) (count non-outliers))))))


; tree structure
; TODO(ELS): defrecord!
(defstruct DataTreeNode :data :children)


(defn create-data-tree-node
  [timing-data]
  (struct-map DataTreeNode
    :data (atom timing-data)    
    :children (atom (hash-map))))