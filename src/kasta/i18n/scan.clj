(ns kasta.i18n.scan
  (:import [java.io File])
  (:require [clojure.walk :as walk]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]

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


(def conform-spec
  (delay
    (try
      (s/describe :cljs.core.specs.alpha/ns-form)
      :cljs.core.specs.alpha/ns-form
      (catch Exception e
        (if (.startsWith (.getMessage e) "Unable to resolve spec")
          :clojure.core.specs.alpha/ns-form
          (throw e))))))


(defn get-ns-aliases [ns-form]
  (let [ns-data   (walk/prewalk reader-cond-unifier ns-form)
        ns-parsed (s/conform @conform-spec (rest ns-data))
        aliases   (for [[_ {:keys [clause body]}]     (:ns-clauses ns-parsed)
                        :when                         (= :require clause)
                        [_ [_ {:keys [lib options]}]] body
                        :when                         (:as options)]
                    [(:as options) lib])]
    aliases))


(defn setup-ns
  "Whole purpose of setup-ns is to return ns which contains necessary aliases so
  that ::ns/keyword will work."
  [ns-form]
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
        edn        (binding [*ns* (setup-ns first-form)]
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
       (filter #(and (source-file? %)
                     (.exists %)))))


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
  (sort-by :filename
    (for [dir  dirs
          file (get-files dir)
          :let [entry (extract-strings file)]
          :when (seq (:strings entry))]
      entry)))


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

(defn find-exe [exe]
  (or (reduce (fn [_ path]
                (let [f (io/file path exe)]
                  (when (.exists f)
                    (reduced (.getAbsolutePath f)))))
        nil
        (into ["/usr/local/opt/gettext/bin/"
               "/usr/local/bin/"
               "/opt/homebrew/bin/"
               "/usr/bin/"]
          (-> (System/getenv "PATH")
              (.split ":"))))
      exe))


(defn exec! [& cmd-and-args]
  (let [proc (.exec (Runtime/getRuntime)
               (into-array String cmd-and-args))]
    (.waitFor proc)
    (if (pos? (.exitValue proc))
      (throw (ex-info (format "%s exited with error code %s"
                        (first cmd-and-args)
                        (.exitValue proc))
               {:stdout (slurp (.getInputStream proc))
                :stderr (slurp (.getErrorStream proc))}))
      (slurp (.getInputStream proc)))))


(defn file->path [file]
  (if (instance? File file)
    (.getAbsolutePath file)
    file))


(def msgmerge-path (delay (find-exe "msgmerge")))
(def msguniq-path (delay (find-exe "msguniq")))


(defn msgmerge! [po-target pot-source]
  (exec! @msgmerge-path "--update" "--backup=off" "--no-wrap" "--sort-output"
    (file->path po-target) (file->path pot-source)))


(defn msguniq! [pot-source]
  (let [path (file->path pot-source)
        dest (str path ".1")]
    (exec! @msguniq-path "-o" dest path)
    (.renameTo (io/file dest) (io/file pot-source))))


(defn update-codebase! [dirs po-target]
  (when-not (.exists (io/file po-target))
    (spit po-target po/PO-INITIAL))
  (let [tmp (File/createTempFile "template" ".pot")]
    (scan-codebase! dirs {:template-file tmp})
    (msguniq! tmp)
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
