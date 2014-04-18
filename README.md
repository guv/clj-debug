# clj-debug

Library for debugging Clojure programms with support for tracing, timing and inspection. More documentation will be written soon.


## Install

Add one of the following to your ```project.clj``` to let Leiningen install ```clj-debug``` for you:

```clj
:dependencies [[clj-debug "0.7.3"]]
:profiles {:dev {:dependencies [[clj-debug "0.7.3"]]}}
```


## TODO

### debug.trace

* variable args not supported, yet
* loop recur not supported, yet
* special treatment for deref-data (copy value when tracing) - similar for lazy-seq (copy realized portion of the lazy-seq)
* improve trace nodes and their gui nodes (not one defrecord per trace node type!), e.g. adjust GUI to reprsent the semantics of the clojure.core macros
* there is still a lot of redundancy in the multimethod implementations for handling known macros
* expression nodes should contain the expression form with filled in parameters


## License

Copyright © 2012-2014 Gunnar Völkel

Distributed under the Eclipse Public License, the same as Clojure.
