(defproject ua.kasta/i18n "1.0.15"
  :description "Internationalization lib"
  :url "https://github.com/kasta-ua/i18n"
  :license {:name "EPL-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1" :scope "provided"]
                 [org.clojure/clojurescript "1.11.60" :scope "provided"]]
  :profiles {:dev {:dependencies [[criterium "0.4.6"]]
                   :repl-options {:init-ns kasta.i18n}}}
  :source-paths ["src"])
