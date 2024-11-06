(ns hnc-gen.term)

(def language :Latin)
; Latin First Year, p6
; Latin Grammar, p5
(defn first-declension-latin
    "Generate Latin words in the first declension."
    ([stem number use-case]
        (case number
            :Singular
            (case use-case
                :Nominative (vector (str stem "a"))
                :Genitive (vector (str stem "ae"))
                :Dative (vector (str stem "ae"))
                :Accusative (vector (str stem "am"))
                :Ablative (vector (str stem "a"))
            )
            :Plural
            (case use-case
                :Nominative (vector (str stem "ae"))
                :Genitive (vector (str stem "arum"))
                :Dative (vector (str stem "is"))
                :Accusative (vector (str stem "as"))
                :Ablative (vector (str stem "is"))
            )
        )
    )
)

(defn plural
    ([word]
        (str word "s")
    )
)

(defn first-declension-english
    "Generate English words in the first declension."
    ([word number use-case]
        (case number
            :Singular
            (case use-case
                :Nominative (vector (str word ", the (a) " word))
                :Genitive (vector (str "of the (a) " word))
                :Dative (vector (str "to or for the (a) " word))
                :Accusative (vector (str "the (a) " word))
                :Ablative (vector (str "by, with, from the (a) " word))
            )
            :Plural
            (case use-case
                (let [words (plural word)]
                    :Nominative (vector (str words ", the " words))
                    :Genitive (vector (str "of the " words))
                    :Dative (vector (str "to or for the" words))
                    :Accusative (vector (str words ", the " words))
                    :Ablative (vector (str "by, with, from the " words))
                )
            )
        )
    )
)

; Latin First Year, p17
(defn second-declension-latin
    "Generate Latin words in the first declension."
    ([stem gender number use-case]
        (case gender
            :Masculine
            (case number
                :Singular
                (case use-case
                    :Nominative (vector (str stem "us"))
                    :Genitive (vector (str stem "i"))
                    :Dative (vector (str stem "o"))
                    :Accusative (vector (str stem "um"))
                    :Ablative (vector (str stem "o"))
                )
                :Plural
                (case use-case
                    :Nominative (vector (str stem "i"))
                    :Genitive (vector (str stem "orum"))
                    :Dative (vector (str stem "is"))
                    :Accusative (vector (str stem "os"))
                    :Ablative (vector (str stem "is"))
                )
            )
            :Neuter
                (case number
                :Singular
                (case use-case
                    :Nominative (vector (str stem "um"))
                    :Genitive (vector (str stem "i"))
                    :Dative (vector (str stem "o"))
                    :Accusative (vector (str stem "um"))
                    :Ablative (vector (str stem "o"))
                )
                :Plural
                (case use-case
                    :Nominative (vector (str stem "a"))
                    :Genitive (vector (str stem "orum"))
                    :Dative (vector (str stem "is"))
                    :Accusative (vector (str stem "a"))
                    :Ablative (vector (str stem "is"))
                )
            )
        )
    )
)

(defn second-declension-english
    "Generate English words in the second declension."
    ([word gender number use-case]
        (first-declension-english word number use-case)
    )
)

; Latin First Year, p7
(defn LAND
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (first-declension-english "land" number use-case)
            :Latin
            (first-declension-latin "terr" number use-case)
        )
    )
)

(defn EARTH
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (first-declension-english "earth" number use-case)
            :Latin
            (first-declension-latin "terr" number use-case)
        )
    )
)

(defn GATE
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (first-declension-english "gate" number use-case)
            :Latin
            (first-declension-latin "port" number use-case)
        )
    )
)

(defn MARY
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (first-declension-english "Mary" number use-case)
            :Latin
            (first-declension-latin "Mari" number use-case)
        )
    )
)

(defn SAILOR
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (first-declension-english "Sailor" number use-case)
            :Latin
            (first-declension-latin "naut" number use-case)
        )
    )
)

(defn VICTORY
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (first-declension-english "victory" number use-case)
            :Latin
            (first-declension-latin "victori" number use-case)
        )
    )
)

(defn FOREST
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (first-declension-english "forest" number use-case)
            :Latin
            (first-declension-latin "silv" number use-case)
        )
    )
)

(defn FAME
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (first-declension-english "fame" number use-case)
            :Latin
            (first-declension-latin "glori" number use-case)
        )
    )
)

(defn GLORY
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (first-declension-english "glory" number use-case)
            :Latin
            (first-declension-latin "glori" number use-case)
        )
    )
)

(defn SINGULAR-3RD
    ([]
        (case language
            :English
            (vector "he, she, it")
            :Latin
            (vector " ")
        )
    )
)

(defn PLURAL-3RD
    ([]
        (case language
            :English
            (vector "they")
            :Latin
            (vector " ")
        )
    )
)

; Latin First Year, p11
(defn PRAY
    ([subject]
        (flatten
            (case language
                :English
                (cond
                    (= subject '(SINGULAR-3RD)) (vector (eval subject) "prays")
                    (= subject '(PLURAL-3RD)) (vector (eval subject) "pray")
                    :else (vector "somebody" "pray")
                )
                :Latin
                (cond
                    (= subject '(SINGULAR-3RD)) (vector "orat")
                    (= subject '(PLURAL-3RD)) (vector "orant")
                    :else (vector "orat")
                )
            )
        )
    )
)

(defn SEE
    ([subject]
        (flatten
            (case language
                :English
                (cond
                    (= subject '(SINGULAR-3RD)) (vector (eval subject) "sees")
                    (= subject '(PLURAL-3RD)) (vector (eval subject) "see")
                    :else (vector "somebody" "sees")
                )
                :Latin
                (cond
                    (= subject '(SINGULAR-3RD)) (vector "videt")
                    (= subject '(PLURAL-3RD)) (vector "vident")
                    :else (vector "videt")
                )
            )
        )
    )
)

(defn NOT
    []
    (case language
        :English (vector "not")
        :Latin (vector "non")
    )
)

; Latin First Year, p14
(defn PRAISE
    ([subject]
        (flatten
            (case language
                :English
                (cond
                    (= subject '(SINGULAR-3RD)) (vector (eval subject) "praises")
                    (= subject '(PLURAL-3RD)) (vector (eval subject) "praise")
                    :else (vector "somebody" "praises")
                )
                :Latin
                (cond
                    (= subject '(SINGULAR-3RD)) (vector "laudat")
                    (= subject '(PLURAL-3RD)) (vector "laudant")
                    :else (vector "laudat")
                )
            )
        )
    )
)

(defn PROVINCE
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (first-declension-english "province" number use-case)
            :Latin
            (first-declension-latin "provinci" number use-case)
        )
    )
)

; Latin First Year, p17
; The second declension
(defn SLAVE
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "slave" :Masculine number use-case)
            :Latin
            (second-declension-latin "serv" :Masculine number use-case)
        )
    )
)

(defn SERVANT
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "servant" :Masculine number use-case)
            :Latin
            (second-declension-latin "serv" :Masculine number use-case)
        )
    )
)

(defn SON
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "son" :Masculine number use-case)
            :Latin
            (second-declension-latin "fili" :Masculine number use-case)
        )
    )
)

(defn GOD
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "God" :Masculine number use-case)
            :Latin
            (second-declension-latin "De" :Masculine number use-case)
        )
    )
)

(defn FRIEND
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "friend" :Masculine number use-case)
            :Latin
            (second-declension-latin "amic" :Masculine number use-case)
        )
    )
)

(defn CHRIST
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "Christ" :Masculine number use-case)
            :Latin
            (second-declension-latin "Christ" :Masculine number use-case)
        )
    )
)

(defn CHRISTIAN
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "Christian" :Masculine number use-case)
            :Latin
            (second-declension-latin "Christian" :Masculine number use-case)
        )
    )
)

(defn AND
    []
    (case language
        :English (vector "and")
        :Latin (vector "et")
    )
)

; Latin First Year, p21
(defn WAR
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "war" :Neuter number use-case)
            :Latin
            (second-declension-latin "bell" :Neuter number use-case)
        )
    )
)

(defn SKY
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "sky" :Neuter number use-case)
            :Latin
            (second-declension-latin "cael" :Neuter number use-case)
        )
    )
)

(defn HEAVEN
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "heaven" :Neuter number use-case)
            :Latin
            (second-declension-latin "cael" :Neuter number use-case)
        )
    )
)

(defn KINGDOM
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "kingdom" :Neuter number use-case)
            :Latin
            (second-declension-latin "regn" :Neuter number use-case)
        )
    )
)

(defn ROYAL-POWER
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "royal power" :Neuter number use-case)
            :Latin
            (second-declension-latin "cael" :Neuter number use-case)
        )
    )
)

(defn REWARD
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "reward" :Neuter number use-case)
            :Latin
            (second-declension-latin "praemi" :Neuter number use-case)
        )
    )
)

(defn DANGER
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "danger" :Neuter number use-case)
            :Latin
            (second-declension-latin "pericul" :Neuter number use-case)
        )
    )
)

(defn COMMAND
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "command" :Neuter number use-case)
            :Latin
            (second-declension-latin "imperi" :Neuter number use-case)
        )
    )
)

(defn POWER
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "power" :Neuter number use-case)
            :Latin
            (second-declension-latin "imperi" :Neuter number use-case)
        )
    )
)

(defn EMPIRE
    ([& {:keys [number use-case]
         :or {number :Singular use-case :Nominative}}]
        (case language
            :English
            (second-declension-english "empire" :Neuter number use-case)
            :Latin
            (second-declension-latin "imperi" :Neuter number use-case)
        )
    )
)

; Latin First Year, p47
(defn BROTHER
    ([]
         (case language
            :English (vector "brother")
            :Latin (vector "frater"))))

(defn FATHER
    ([]
         (case language
            :English (vector "father")
            :Latin (vector "pater"))))

(defn MOTHER
    ([]
         (case language
            :English (vector "mother")
            :Latin (vector "mater"))))

(defn MOUNTAIN
    ([]
         (case language
            :English (vector "mountain")
            :Latin (vector "mons"))))

(defn SHOUT
    ([]
         (case language
            :English (vector "shout")
            :Latin (vector "clamor"))))

(defn CHIEF
    ([]
         (case language
            :English (vector "chief")
            :Latin (vector "princeps"))))

(def language :English)
(EARTH {:number :Singular :use-case :Nominative})
(LAND {:number :Singular :use-case :Nominative})
(GATE {:number :Singular :use-case :Nominative})
(MARY {:number :Singular :use-case :Nominative})
(SAILOR {:number :Singular :use-case :Nominative})
(VICTORY {:number :Singular :use-case :Nominative})
(FOREST {:number :Singular :use-case :Nominative})
(FAME {:number :Singular :use-case :Nominative})
(GLORY {:number :Singular :use-case :Nominative})

; p11
(def language :Latin)
(PRAY '(SINGLAR-3RD))
(PRAY '(PLURAL-3RD))
(SEE '(SINGLAR-3RD))
(SEE '(PLURAL-3RD))
(NOT)

; p14
(PRAISE '(SINGLAR-3RD))
(PRAISE '(PLURAL-3RD))
(PROVINCE {:number :Singular :use-case :Nominative})

; p17
(SLAVE {:number :Singular :use-case :Nominative})
(SERVANT {:number :Singular :use-case :Nominative})
(SON {:number :Singular :use-case :Nominative})
(GOD {:number :Singular :use-case :Nominative})
(FRIEND {:number :Singular :use-case :Nominative})
(CHRIST {:number :Singular :use-case :Nominative})
(CHRISTIAN {:number :Singular :use-case :Nominative})
(AND)

; p21
(WAR {:number :Singular :use-case :Nominative})
(SKY {:number :Singular :use-case :Nominative})
(HEAVEN {:number :Singular :use-case :Nominative})
(KINGDOM {:number :Singular :use-case :Nominative})
(ROYAL-POWER {:number :Singular :use-case :Nominative})
(REWARD {:number :Singular :use-case :Nominative})
(DANGER {:number :Singular :use-case :Nominative})
(COMMAND {:number :Singular :use-case :Nominative})
(POWER {:number :Singular :use-case :Nominative})
(EMPIRE {:number :Singular :use-case :Nominative})

