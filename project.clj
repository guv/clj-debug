(defproject clj-debug "0.6.4"
  :description "Library for debugging Clojure programms with support for tracing, timing and inspection."
  :url "https://github.com/guv/clj-debug"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
	:dependencies 
	[[org.clojure/clojure "1.2.1"]
   [org.clojure/core.incubator "0.1.0"]
   [clj-gui "0.2.1"]]
  :aot [debug.inspect.inspectable])
