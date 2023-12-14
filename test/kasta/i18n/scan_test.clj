(ns kasta.i18n.scan-test
  (:require [clojure.test :as t]
            [kasta.i18n.scan :as scan]))

(def cljs-ns-form
  '(ns com.example.ns
     (:require-macros [com.example.ns])
     (:require [clojure.string :as str])))

(t/deftest get-ns-aliases
  (t/testing "getting ns aliases with :require-macros"
    (t/is (= '([str [:sym clojure.string]]) (scan/get-ns-aliases cljs-ns-form)))))
