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
            (if (= (first x) 'HEAVEN)
              (make-phase (flatten (vector (sci-eval x) (sci-eval y))))
              (make-phase (flatten (vector (sci-eval x) "和" (sci-eval y)))))
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

(def s1 '(CREATE '(GOD) '(AND '(HEAVEN :number "Plural" :spec "Definite") '(EARTH)) :tense "Past" :time '(BEGIN)))

(defn LORD []
  (case @language
    :English
    "Lord"
    :Chinese
    "聖"))

(defn CHRIST []
  (case @language
    :English
    "Christ"
    :Chinese
    "主基督"))

(defn IN-YOUR-HEART []
  (case @language
    :English
    "in your hearts"
    :Chinese
    "心裡"))

(defn BUT []
  (case @language
    :English
    "but"
    :Chinese
    "只要"))

(defn REVERE1 [subject object & {:keys [target location connect]
                  :or {target nil location nil connect nil}}]
  (case @language
    :English
    (make-sentence (vector (sci-eval connect)
                           (sci-eval location) (sci-eval subject)
                           "revere" (sci-eval object) "as" (sci-eval target)))
    ;; "But in your hearts revere Christ as Lord."
    :Chinese
    (make-sentence (vector (sci-eval connect) (sci-eval location)
                           (sci-eval subject) "尊" (sci-eval object) "為" (sci-eval target)))))
    ;; "只要心裡尊主基督為聖。"))

(defn ALWAYS []
  (case @language
    :English
    "always"
    :Chinese
    "常"))

(defn GIVE-ANSWER []
  (case @language
    :English
    "give an answer to everyone who asks you to give the reason for the hope that you have"
    :Chinese
    "有人問你們心中盼望的緣由"))

(defn PREPARE1 [subject object & {:keys [target frequency]
                  :or {target nil frequency nil}}]
  (case @language
    :English
    (make-sentence (vector (sci-eval frequency) "be prepared"
                           "to" (sci-eval target)))
    ;; "Always be prepared to give an answer to everyone who asks you to give the reason for the hope that you have."

    :Chinese
    (make-sentence (vector (sci-eval target) "," "就要" (sci-eval frequency) "作準備"))))
    ;; "有人問你們心中盼望的緣由，就要常作準備，"

(defn GENTLENESS-AND-RESPECT []
  (case @language
    :English
    "gentleness and respect"
    :Chinese
    "溫柔、敬畏的心"))

(defn DO-THIS [subject object & {:keys [attitude]
                  :or {attitude nil}}]
  (case @language
    :English
    (make-sentence (vector "but do this" "with" (sci-eval attitude)))
    ;; "But do this with gentleness and respect."
    :Chinese
    (make-sentence (vector "以" (sci-eval attitude) "回答各人"))))
    ;; "以溫柔、敬畏的心回答各人。"

(defn REVERE []
  (case @language
    :English
    "But in your hearts revere Christ as Lord."
    :Chinese
    "只要心裡尊主基督為聖。"))

(defn PREPARE []
  (case @language
    :English
    "Always be prepared to give an answer to everyone who asks you to give the reason for the hope that you have."
    :Chinese
    "有人問你們心中盼望的緣由，就要常作準備，"))

(defn DO []
  (case @language
    :English
    "But do this with gentleness and respect."
    :Chinese
    "以溫柔、敬畏的心回答各人。"))

(defn COME []
  (case @language
    :English
    "and humility comes before honor."
    :Chinese
    "尊榮以前，必有謙卑。"))

(defn BE []
  (case @language
    :English
    "Wisdom’s instruction is to fear the Lord,"
    :Chinese
    "敬畏耶和華是智慧的訓誨；"))

(defn CON
  ([x y]
  (case @language
    :English
    (make-phase (map sci-eval [x y]))
    :Chinese
    (make-phase (map sci-eval [x y]))))
  ([x y z]
  (case @language
    :English
    (make-phase (map sci-eval [x y z]))
    :Chinese
    (make-phase (map sci-eval [x y z])))))

(def sci-context
  {:namespaces {'user (into {} (map (fn [[k v]] [k @v]) (ns-publics 'hnc-gen.bible)))}})

(def daily-bread [{:title "Genesis 1:1"
                   :verse '(CREATE '(GOD) '(AND '(HEAVEN :number "Plural" :spec "Definite") '(EARTH)) :tense "Past" :time '(BEGIN))
                   :image "/img/genesis-1-1.jpg"
                   :audio-en "/audio/en/genesis-1-1.m4a"
                   :audio-cn "/audio/cn/genesis-1-1.m4a"}
                  {:title "1 Peter 3:15"
                   ;; :verse '(CON '(REVERE) '(PREPARE) '(DO))
                   :verse '(CON '(REVERE1 nil '(CHRIST) :target '(LORD) :location '(IN-YOUR-HEART) :connect '(BUT))
                                '(PREPARE1 nil nil :target '(GIVE-ANSWER) :frequency '(ALWAYS))
                                '(DO-THIS nil nil :attitude '(GENTLENESS-AND-RESPECT)))
                   :image "/img/1-peter-3-15.jpg"
                   :audio-en "/audio/en/1-peter-3-15.m4a"
                   :audio-cn "/audio/cn/1-peter-3-15.m4a"}
                  {:title "Proverbs 15:33"
                   :verse '(CON '(BE) '(COME))
                   :image "/img/proverbs-15-33.jpg"
                   :audio-en "/audio/en/proverbs-15-33.m4a"
                   :audio-cn "/audio/cn/proverbs-15-33.m4a"}
                  ]
  )
;; (CREATE '(GOD) '(AND '(HEAVEN {:number "Plural" :spec "Definite"}) '(EARTH)) {:tense "Past" :time '(BEGIN)})

