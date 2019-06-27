(ns kasta.i18n.po-test
  (:require [clojure.test :as t]
            [clojure.java.io :as io]

            [kasta.i18n.po :as po]))


(defn test-data [name]
  (slurp (io/file (str "test/fixtures/" name))))


(t/deftest reading-po
  (t/testing "reading po file"
    (let [single (test-data "single.po")]
      (t/is (= {::po/msgid  '(utils/plural warehouses " отделение" " отделения" " отделений")
                ::po/msgstr '(utils/plural warehouses " відділення" " відділення" " відділень")}
               (po/parse-block single))))))

