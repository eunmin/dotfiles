{:user {:plugins [[cider/cider-nrepl "0.10.0-SNAPSHOT"]
       		  [refactor-nrepl "2.0.0-SNAPSHOT"]
                  [lein-environ "1.0.0"]]
	:dependencies [[acyclic/squiggly-clojure "0.1.4"]
                       [org.clojure/tools.nrepl "0.2.12"]]}
 :env {:squiggly {:checkers [:eastwood :typed :kbit]
                  :eastwood-exclude-linters [:unlimited-use]}}}
