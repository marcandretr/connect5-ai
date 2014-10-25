(defproject connect5-ai "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [com.google.collections/google-collections "1.0"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [org.clojure/math.combinatorics "0.0.8"]]
  :plugins [[codox "0.8.10"]]
    :codox {:defaults {:doc/format :markdown}}
  :java-source-paths ["src-java"]
  ;:main ^:skip-aot connect5-ai.core
  :aot :all
  :target-path "target/%s"
  :omit-source true
  :profiles {:uberjar {:aot :all}})
