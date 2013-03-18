(defproject clj-debug "0.7.0"
  :description "Library for debugging Clojure programms with support for tracing, timing and inspection."
  :url "https://github.com/guv/clj-debug"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.0.0"
	:dependencies 
	[[org.clojure/clojure "1.5.1"]
   [org.clojure/core.incubator "0.1.2"]
   [clj-gui "0.3.0"]]
  :aot [debug.inspect.inspectable])
