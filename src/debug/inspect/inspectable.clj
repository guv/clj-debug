(ns debug.inspect.inspectable)

(defprotocol Inspectable
  (attribute-map [this]))

