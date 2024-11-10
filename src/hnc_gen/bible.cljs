(ns hnc-gen.bible
  (:require [cljs.js :as cljs]
            [sci.core :as sci]))

(declare sci-context make-phase)

;;(def sci-context
;;  {:namespaces {'user {'GOD hnc-gen.bible/GOD}}})
(defn process-list [input]
  (if (and (list? input) (= 'quote (first input)))
    (second input)
    input))

(defn evaluate [code]
  (try
    (let [result (sci/eval-string code sci-context)]
      result)
    (catch js/Error e
      (println "Error:" (.-message e)))))

(defn sci-eval [list]
  (evaluate (str (process-list list))))

(defn generate [list]
  (make-phase (sci-eval list)))

(def language (atom :English))

(defn is-single?
    [lst]
    (if (and (seq lst) (= (first lst) 'GOD))
        true
        false
    )
)

(defn make-sentence [words]
    (let
        [first-word (first words)
            capitalized-first (str (clojure.string/upper-case (subs first-word 0 1))
                               (subs first-word 1))
            rest-words (rest words)
        ]
        (str (clojure.string/join " " (cons capitalized-first rest-words)) ".")
    )
)

(defn make-phase [words]
  (clojure.string/join " " words))

(defn CREATE
    ([]
        (case @language
            :English
            (make-phase (vector "create"))
            :Chinese
            (make-phase (vector "创造"))
        )
    )
    ([creator object & {:keys [tense time]
                  :or {tense "Now" time nil}}]

         (case @language
             :English
               (make-sentence
                 (flatten
                    (vector
                        (if (nil? time) [] (vector (sci-eval time)))
                        (case tense
                          "Now"
                          (flatten (vector (sci-eval creator) (if (is-single? creator) "creates" "create") (sci-eval object)))
                          "Past"
                          (flatten (vector (sci-eval creator) (if (is-single? creator) "created" "create") (sci-eval object)))
                          )
                    )
                 )
               )
             :Chinese
               (make-sentence
                 (flatten
                    (vector
                        (if (nil? time) [] (vector (sci-eval time)))
                          (flatten (vector (sci-eval creator) (CREATE) (sci-eval object)))
                    )
                  )
                )
        )
    )
)

(defn GOD
    ([]
        (case @language
            :English
            (make-phase (vector "God"))
            :Chinese
            (make-phase (vector "神"))
        )
    )
)

(defn BEGIN
    ([]
        (case @language
            :English
            (make-phase (vector "in the beginning"))
            :Chinese
            (make-phase (vector "起初"))
        )
    )
)

(defn HEAVEN
    ([& {:keys [number spec]
         :or {number "Singular" spec "Definite"}}]
        (case @language
            :English
            (case number
                "Singular"
                (case spec
                    "Definite"
                    (make-phase (vector "the heaven"))
                    "Indefinite"
                    (make-phase (vector "a heaven"))
                )
                "Plural"
                (case spec
                    "Definite"
                    (make-phase (vector "the heavens"))
                    "Indefinite"
                    (make-phase (vector "heavens"))
                )
            )
            :Chinese
            (make-phase (vector "天"))
        )
    )
)

(defn EARTH
    [& {:keys [number spec]
         :or {number "Singular" spec "Definite"}}]
    (case @language
        :English
        (case number
            "Singular"
            (case spec
                "Definite"
                (make-phase (vector "the earth"))
                "Indefinite"
                (make-phase (vector "an earth"))
            )
            "Plural"
            (case spec
                "Definite"
                (make-phase (vector "the earths"))
                "Indefinite"
                (make-phase (vector "earths"))
            )
        )
        :Chinese
        (make-phase (vector "地"))
    )
)

(defn AND
    ([x,y]
        (case @language
            :English
            (make-phase (flatten (vector (sci-eval x) "and" (sci-eval y))))
            :Chinese
            (make-phase (flatten (vector (sci-eval x) "和" (sci-eval y))))
        )
    )
)

;; (def @language :English)
(defn BOOK-BIBLE
    []
    (vector
;        (CREATE '(GOD) '(AND '(HEAVEN) '(EARTH)))
        (CREATE '(GOD) '(AND '(HEAVEN {:number "Plural" :spec "Definite"}) '(EARTH)) {:tense "Past" :time '(BEGIN)})
    )
)

(defn Chinese []
  (reset! language :Chinese))

(defn English []
  (reset! language :English))

(def sci-context
  {:namespaces {'user (into {} (map (fn [[k v]] [k @v]) (ns-publics 'hnc-gen.bible)))}})

(def s1 '(CREATE '(GOD) '(AND '(HEAVEN :number "Plural" :spec "Definite") '(EARTH)) :tense "Past" :time '(BEGIN)))
;; (CREATE '(GOD) '(AND '(HEAVEN {:number "Plural" :spec "Definite"}) '(EARTH)) {:tense "Past" :time '(BEGIN)})

