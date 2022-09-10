(defproject awbc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]

                 ;; shadow
                 [thheller/shadow-cljs "2.16.7"]

                 ;; cljs
                 [reagent "0.8.1"]
                 [garden "1.3.9"]
                 [re-frame "0.10.6"]
                 [metosin/malli "0.6.2"]
                 [com.cognitect/transit-cljs "0.8.256"]
                 [re-pressed "0.3.1"]
                 [lambdaisland/deep-diff2 "2.4.138"]

                 ;; clojure
                 [http-kit "2.3.0"]
                 [mount "0.1.16"]
                 [compojure "1.6.1"]
                 [com.cognitect/transit-clj "0.8.313"]
                 [environ "1.1.0"]
                 [ring "1.7.1"]
                 [clj-time "0.15.1"]
                 [ring/ring-defaults "0.3.2"]
                 [cheshire "5.8.1"]

                 [com.phronemophobic/membrane  "0.9.31.6-beta"]]
  :main ^:skip-aot awbc.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
