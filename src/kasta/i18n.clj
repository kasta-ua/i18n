(ns kasta.i18n
  (:import [java.nio.file Paths])
  (:require [clojure.string :as str]

            [kasta.i18n.scan :as scan]
            [kasta.i18n.po :as po]))


;;; Config

(def LANGS (some-> (or (System/getenv "KASTA_I18N")
                       (System/getProperty "kasta.i18n"))
             (str/split #", ")))

(def PO-DIR (or (System/getenv "KASTA_I18N_DIR")
                (System/getProperty "kasta.i18n.dir")
                "resources/i18n"))


;;; Language switch

(def ^:dynamic *lang* nil)


(defmacro with-lang [lang & body]
  `(binding [*lang* ~lang]
     ~@body))


;;; Finding translations

(def read-translations
  (memoize
    (fn [lang]
      (po/read-po (Paths/get PO-DIR (into-array [(str lang ".po")]))))))


(defn get-trans [lang input]
  (or (get (read-translations lang) input)
      input))


;;; Data reader

(defn cljs-t [input]
  (if *lang*
    (get-trans *lang* input)
    input))


(defn clj-t [input]
  (if (seq LANGS)
    `(case *lang*
       ~@(mapcat (fn [lang] [lang (get-trans lang input)]) LANGS)
       ~input)
    input))


(defmacro t [input]
  (if (:ns &env)
    (cljs-t input)
    (clj-t input)))


(defn reader-t [input]
  `(t ~input))


;;;

(def scan-codebase! scan/scan-codebase!)
(def update-codebase! scan/update-codebase!)
(def read-po po/read-po)