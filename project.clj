(defproject word-it "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/data.generators "0.1.2"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/core.async "0.2.374"]
                 [me.raynes/fs "1.4.6"]]
  :main ^:skip-aot word-it.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
