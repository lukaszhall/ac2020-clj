(defproject ac2020-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [
                 [org.clojure/clojure "1.10.3"]
                 [org.clojure/core.logic "1.0.0"]

                 [org.clojure/test.check "0.10.0"]
                 [orchestra "2020.07.12-1"]

                 [io.github.nextjournal/clerk "0.5.346"]

                 ]
  :main ^:skip-aot ac2020-clj.core
  :target-path "target/%s"

  ;:mirrors {"central" {:name "Central"
  ;                     :url  "https://repository.walmart.com/content/groups/public/"}
  ;          "clojars" {:name         "Internal nexus Clojars"
  ;                     :url          "https://repository.walmart.com/content/repositories/clojars/"
  ;                     :repo-manager true}}

  :profiles {:uberjar {:aot :all}})
