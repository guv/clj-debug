(ns debug.inspect-test
  (:use debug.inspect))

(inspection-setup Bla)

(deftype Bla [a b]
  Object
  (toString [this] 
    (str "a = ", a, " b = ", b)))
