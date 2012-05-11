# clj-debug

Library for debugging Clojure programms. More documentation will be written soon.


## Install

Add one of the following to your ```project.clj``` to let Leiningen install ```clj-debug``` for you:

```clj
:dependencies [[clj-debug "0.5.2"]]
:dev-dependencies [[clj-debug "0.5.2"]]
```


## TODO

* remove all clojure.contrib references

### debug.trace

* variable args not supported, yet
* loop recur not supported, yet
* special treatment for deref-data (copy value when tracing) - similar for lazy-seq (copy realized portion of the lazy-seq)
* improve trace nodes and their gui nodes (not one defrecord per trace node type!), e.g. adjust GUI to reprsent the semantics of the clojure.core macros
* there is still a lot of redundancy in the multimethod implementations for handling known macros
* expression nodes should contain the expression form with filled in parameters

### debug.intercept

* *-setup throws an Exception on reevaluation in REPL (via CCW CTRL+ALT+S; most likely also in general)

## License

Copyright © 2012 Gunnar Völkel

Distributed under the Eclipse Public License, the same as Clojure.