(ns kasta.i18n.scan
  (:import [java.io File])
  (:require [clojure.walk :as walk]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.spec.alpha :as s]
            [cljs.core.specs.alpha :as sa]

            [kasta.i18n.po :as po]))


(defn parse-reader-cond
  "Either form is reader conditional and then a list of all forms (for any case)
  is returned or it is not and then a single item list is returned with a form."
  [form]
  (if (instance? clojure.lang.ReaderConditional form)
    (let [data (->> (:form form)
                    (partition 2)
                    (map second))]
      (if (:splicing? form)
        (apply concat data)
        data))
    (cons form nil)))


(defn reader-cond-unifier
  "Looks into forms to parse reader conditional tags.

  Unfortunately directly parsing reader conditionals does not work as you can't
  splice from inside, so you have to do your own loop (instead of relying on
  prewalk)."
  [form]
  (if (list? form)
    (mapcat parse-reader-cond form)
    form))


(defn get-ns-aliases [ns-form]
  (let [ns-form  (walk/prewalk reader-cond-unifier ns-form)
        parsed   (s/conform ::sa/ns-form (rest ns-form))
        requires (filter #(= :require (first %)) (:ns-clauses parsed))
        requires (mapcat (comp :body second) requires)
        aliases  (->> requires
                      (map (comp second second))
                      (map (juxt #(-> % :options :as) #(-> % :lib second)))
                      (filterv #(every? some? %))
                      ;; map deduplicates aliases
                      (into {}))]
    aliases))


(defn setup-ns
  "Whole purpose of setup-ns is to return ns which contains necessary aliases so
  that ::ns/keyword will work."
  [file ns-form]
  (let [inner-ns (create-ns 'kasta.i18n.inner)]

    ;; remove existing aliases
    (doseq [[alias-sym _] (ns-aliases inner-ns)]
      (ns-unalias inner-ns alias-sym))

    (when (and (list? ns-form)
               (= 'ns (first ns-form)))
      (let [aliases (get-ns-aliases ns-form)]
        (doseq [[alias-sym ns-sym] aliases
                :let [target-ns (try (the-ns ns-sym)
                                     ;; if we can't find namespace, replace it
                                     ;; with something known
                                     (catch Exception e
                                       inner-ns))]]
          (.addAlias inner-ns alias-sym target-ns))))
    inner-ns))


(defn visitor [c-form! form]
  (cond
    (instance? clojure.lang.ReaderConditional form)
    (:form form)

    (instance? clojure.lang.TaggedLiteral form)
    (c-form! (:tag form) (:form form))

    :else
    form))


(defn parse! [file collect!]
  (let [c-form! (fn [tag value]
                  (when (= tag 't)
                    (collect! value)))

        ;; named `rread` just to be different from `read`
        rread #(binding [*read-eval*    false
                         *data-readers* (assoc *data-readers*
                                          't collect!
                                          'js identity)]
                 (clojure.core/read {:eof ::eof, :read-cond :preserve} %))

        rdr        (-> file io/reader java.io.PushbackReader.)
        first-form (rread rdr)
        edn        (binding [*ns* (setup-ns file first-form)]
                     (->> (repeatedly #(rread rdr))
                          (take-while #(not= % ::eof))
                          (into [first-form])))]

    (.close rdr)

    (walk/prewalk #(visitor c-form! %) edn)))


;;; Scanning

(def SOURCE-RE #"\.clj[cs]?$")


(defn source-file? [file]
  (re-find SOURCE-RE (.getName file)))


(defn get-files [dir]
  (->> (file-seq (io/file dir))
       (filter source-file?)
       (filter #(.exists %))))


(defn extract-strings [file]
  (let [!strings (volatile! [])
        collect! (fn [value]
                   (vswap! !strings conj {:value value
                                          :notes (:notes (meta value))})
                   ;; data reader should never return nil, because that makes
                   ;; Clojure fail with 'No dispatch macro for: t'
                   value)]

    (parse! file collect!)

    {:filename (io/as-relative-path file)
     :strings  @!strings}))


(defn scan-files [dirs]
  (->> (mapcat get-files dirs)
       (map extract-strings)
       (filter (comp seq :strings))
       (sort-by :filename)))


(defn scan-codebase! [dirs
                      & [{:keys [template-file]
                           :or   {template-file "resources/i18n/template.pot"}}]]
  (assert (seq dirs) "First argument should be a seq of directories to scan")
  (let [tmpl    (io/file template-file)
        results (scan-files dirs)
        po      (po/gen-po results)]
    (io/make-parents tmpl)
    (spit tmpl po)))


;;; Msgmerge

(def msgmerge-path
  (delay
    (reduce (fn [_ x]
              (when (.exists (io/file x))
                (reduced x)))
      nil
      ["/usr/local/opt/gettext/bin/msgmerge"
       "/usr/local/bin/msgmerge"
       "/usr/bin/msgmerge"])))

(defn file->path [file]
  (if (instance? File file)
    (.getAbsolutePath file)
    file))


(defn msgmerge! [po-target pot-source]
  (shell/sh @msgmerge-path "--update" "--backup=off"
    (file->path po-target)
    (file->path pot-source)))


(defn update-codebase! [dirs po-target]
  (let [tmp (File/createTempFile "template" "pot")]
    (scan-codebase! dirs {:template-file tmp})
    (msgmerge! po-target tmp)
    (io/delete-file tmp)))


(comment
  (extract-strings (io/file "src/mk/fe/common/header.cljc"))
  (extract-strings (io/file "src/mk/common/time.cljc"))
  (extract-strings (io/file "src/mk/fe/main.cljs"))
  (extract-strings (io/file "src/mk/fe/catalogue/views.cljc"))
  (extract-strings (io/file "qwe.clj"))

  (scan-files ["src"])
  (scan-codebase! ["src"] {:template-file "resources/i18n/uk.pot"}))
