(ns debug.inspect.type)


(defmacro with-type
  [typekey, obj-creating-expr]
 `(vary-meta ~obj-creating-expr assoc :type ~typekey))