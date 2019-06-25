(ns kasta.i18n.po
  (:import [java.time Instant])
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;;; Utils

(defn fmt-escaped [value]
  (if (string? value)
    value
    (str "~" (pr-str value))))


(defn read-escaped [value]
  ;; this looks like double-encoding since it is: first we generate string by
  ;; `fmt-escaped`, and then it's put in double-quotes in `gen-po`
  (let [value (edn/read-string value)]
    (if (= \~ (first value))
      (edn/read-string (subs value 1))
      value)))


;;; Generation

(def po-title
  "# SOME DESCRIPTIVE TITLE.
# Copyright (C) YEAR Free Software Foundation, Inc.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
msgid \"\"
msgstr \"\"
\"PO-Revision-Date: %s\\n\"
\"Content-Type: text/plain; charset=UTF-8\\n\"

")


(defn gen-po [results]
  (let [entries (->> results
                     (mapcat #(map vector (repeat (:filename %)) (:strings %))))]
    (str
      (format po-title (Instant/now))
      (str/join "\n"
        (for [[filename entry] entries]
          (str
            (when (:notes entry)
              (format "#. %s\n" (:notes entry)))
            (format "#: %s\n" filename)
            (format "msgid %s\n" (pr-str (fmt-escaped (:value entry))))
            "msgstr \"\"\n"))))))


;;; Reading

(defn parse-block
  "Parses a block in a PO file, and returns an object with the msgid,
  msgstr and possible plural versions of the strings."
  [block]
  (let [data (->> (map str/trim (str/split-lines block))
                  (map #(str/split % #"\s" 2))
                  (into {}))]
    [(-> (get data "msgid")  read-escaped)
     (-> (get data "msgstr") read-escaped)]))


(defn- ->kv [block]
  (if (contains? block ::msgid-plural)
    [[(::msgid block) (::msgid-plural block)]
     [(::msgstr block) (::msgstr-plural block)]]
    [(::msgid block) (::msgstr block)]))


(defn read-po [file]
  (->> (str/split (slurp file) #"\n\n")
       (drop 1) ;; Header meta data
       (map parse-block)
       (into {})))


(comment
   (read-po "resources/i18n/uk.po"))
