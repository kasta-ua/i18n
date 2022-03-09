(ns kasta.i18n
  (:import [java.nio.file Paths])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]

            [kasta.i18n.scan :as scan]
            [kasta.i18n.po :as po]))


;;; Config

(def LANGS (some-> (or (System/getenv "KASTA_I18N")
                       (System/getProperty "kasta.i18n"))
             (str/split #", ")))

(def PO-DIR (or (System/getenv "KASTA_I18N_DIR")
                (System/getProperty "kasta.i18n.dir")
                "i18n"))


;;; Language switch

(def ^:dynamic *lang* nil)


(defmacro with-lang [lang & body]
  `(binding [*lang* ~lang]
     ~@body))


;;; Finding translations

(def TRANSLATIONS (atom {}))


(def read-translations
  (fn [lang]
    (if-let [trans (find @TRANSLATIONS lang)]
      (val trans)
      (let [path (Paths/get PO-DIR (into-array [(str lang ".po")]))
            trans (po/read-po (io/resource (str path)))]
        (swap! TRANSLATIONS assoc lang trans)
        trans))))


(defn get-trans [lang input]
  (or (not-empty (get (read-translations lang) input))
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
