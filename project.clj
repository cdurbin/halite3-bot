(defproject starter-bot "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [cheshire "5.8.1"]
                 [clj-time "0.15.0"]]
  :main ^:skip-aot hlt.core
  :target-path "target/%s/"
  :uberjar-name "MyBot.jar"
  :jvm-opts ["-Dclojure.compiler.direct-linking=true"]
  ;; Directory in which to place AOT-compiled files. Including %s will
  ;; splice the :target-path into this value.
  :compile-path "%s/classy-files"
  :clean-targets [:target-path :compile-path]
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/tools.namespace "0.2.11"]
                                  ; [org.clojars.gjahad/debug-repl "0.3.3"]
                                  [proto-repl "0.3.1"]
                                  [criterium "0.4.4"]]
                   :jvm-opts ^:replace ["-Dcom.sun.management.jmxremote"
                                        "-Dcom.sun.management.jmxremote.ssl=false"
                                        "-Dcom.sun.management.jmxremote.authenticate=false"
                                        "-Dcom.sun.management.jmxremote.port=1198"]
                   :source-paths ["src" "dev" "test"]}})
