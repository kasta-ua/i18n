(ns kasta.i18n.po
  "Reading PO files originally from
  https://github.com/brightin/pottery/blob/master/src/pottery/po.clj"
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

(defn- quoted-string? [line]
  (and (str/starts-with? line "\"") (str/ends-with? line "\"")))


(defn- read-quoted-string [line]
  (str/replace
   (subs line 1 (dec (count line)))
   #"\\n" "\n"))


(defn empty-str? [s]
  (= s "\"\""))


(defn- tag-parser
  "Creates a fn that takes remaining lines and returns a tuple with
  the key, value and and the remaining unparsed lines."
  [key]
  (fn [[line & rest]]
    (let [[multiline-values rest] (split-with quoted-string? rest)
          values                  (->> (concat [line] multiline-values)
                                       (remove empty-str?))]
      [key (read-escaped (str/join values)) rest])))


(defn default-parser [lines]
  [nil nil (drop 1 lines)])


(def PO_TAGS
  {"msgid"        (tag-parser ::msgid)
   "msgid_plural" (tag-parser ::msgid-plural)
   "msgstr"       (tag-parser ::msgstr)
   "msgstr[0]"    (tag-parser ::msgstr)
   "msgstr[1]"    (tag-parser ::msgstr-plural)})


(defn parse-block
  "Parses a block in a PO file, and returns an object with the msgid,
  msgstr and possible plural versions of the strings."
  [block-str]
  (loop [lines (map str/trim (str/split-lines block-str))
         result {}]
    (if (empty? lines)
      result
      (let [[tag remainder] (str/split (first lines) #"\s" 2)
            lines (concat [remainder] (rest lines))
            parser (get PO_TAGS tag default-parser)
            [k v rest] (parser lines)]
        (recur rest (if v (assoc result k v) result))))))


(defn- ->kv [block]
  (if (contains? block ::msgid-plural)
    [[(::msgid block) (::msgid-plural block)]
     [(::msgstr block) (::msgstr-plural block)]]
    [(::msgid block) (::msgstr block)]))


(defn read-po [file]
  (->> (str/split (slurp file) #"\n\n")
       (drop 1) ;; Header meta data
       (map parse-block)
       (map ->kv)
       (into {})))


(comment
   (read-po "resources/i18n/uk.po"))
