(defproject ua.kasta/i18n "1.0.11"
  :description "Internationalization lib"
  :url "https://github.com/kasta-ua/i18n"
  :license {:name "EPL-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3" :scope "provided"]
                 [org.clojure/clojurescript "1.11.4" :scope "provided"]]
  :repl-options {:init-ns kasta.i18n}
  :source-paths ["src"])
