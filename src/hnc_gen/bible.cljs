(ns hnc-gen.bible
  (:require [cljs.js :as cljs]
            [sci.core :as sci]
            [cljs-time.core :as t]))

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

(defn remove-all-spaces
  [s]
  (clojure.string/replace s #"\s" ""))

(defn Hebrews-13-1-2 []
  (case @language
    :English
    "Keep on loving one another as brothers and sisters. Do not forget to show hospitality to strangers, for by so doing some people have shown hospitality to angels without knowing it."
    :Chinese
    (remove-all-spaces "你 們 務 要 常 存 弟 兄 相 愛 的 心 。 不 可 忘 記 用 愛 心 接 待 客 旅 ； 因 為 曾 有 接 待 客 旅 的 ， 不 知 不 覺 就 接 待 了 天 使 。")
    ))

(defn Corinthians1-15-33 []
  (case @language
    :English
    "Do not be misled: “Bad company corrupts good character.”"
    :Chinese
    (remove-all-spaces "你 們 不 要 自 欺 ； 濫 交 是 敗 壞 善 行 。")
    ))

(defn Psalm-120-2 []
  (case @language
    :English
    "Save me, Lord, from lying lips and from deceitful tongues."
    :Chinese
    (remove-all-spaces "耶 和 華 啊 ， 求 你 救 我 脫 離 說 謊 的 嘴 唇 和 詭 詐 的 舌 頭 ！")
    ))

(defn Deuteronomy-4-39 []
  (case @language
    :English
    "Acknowledge and take to heart this day that the Lord is God in heaven above and on the earth below. There is no other."
    :Chinese
    (remove-all-spaces "所 以 ， 今 日 你 要 知 道 ， 也 要 記 在 心 上 ， 天 上 地 下 惟 有 耶 和 華 他 是 神 ， 除 他 以 外 ， 再 無 別 神 。")
    ))

(defn Luke-9-24 []
  (case @language
    :English
    "For whoever wants to save their life will lose it, but whoever loses their life for me will save it."
    :Chinese
    (remove-all-spaces "因 為 ， 凡 要 救 自 己 生 命 的 ， 必 喪 掉 生 命 ； 凡 為 我 喪 掉 生 命 的 ， 必 救 了 生 命 。")
    ))

(defn Titus-3-5 []
  (case @language
    :English
    "He saved us, not because of righteous things we had done, but because of his mercy. He saved us through the washing of rebirth and renewal by the Holy Spirit."
    :Chinese
    (remove-all-spaces "他 便 救 了 我 們 ； 並 不 是 因 我 們 自 己 所 行 的 義 ， 乃 是 照 他 的 憐 憫 ， 藉 著 重 生 的 洗 和 聖 靈 的 更 新 。")
    ))

(defn Nahum-1-3 []
  (case @language
    :English
    "The Lord is slow to anger but great in power;
the Lord will not leave the guilty unpunished.
His way is in the whirlwind and the storm,
and clouds are the dust of his feet."
    :Chinese
    (remove-all-spaces "耶 和 華 不 輕 易 發 怒 ， 大 有 能 力 ， 萬 不 以 有 罪 的 為 無 罪 。 他 乘 旋 風 和 暴 風 而 來 ， 雲 彩 為 他 腳 下 的 塵 土 。")
    ))

(defn Matthew-24-14 []
  (case @language
    :English
    "And this gospel of the kingdom will be preached in the whole world as a testimony to all nations, and then the end will come."
    :Chinese
    (remove-all-spaces "這 天 國 的 福 音 要 傳 遍 天 下 ， 對 萬 民 作 見 證 ， 然 後 末 期 才 來 到 。")
    ))

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

                  {:date (t/date-time 2024 11 10)
                   :title "Hebrews 13:1-2"
                   :verse '(Hebrews-13-1-2)
                   :image "/img/hebrews-13-1-2.jpg"
                   :audio-en "/audio/en/hebrews-13-1-2-en.m4a"
                   :audio-cn "/audio/cn/hebrews-13-1-2-cn.m4a"}

                  {:date (t/date-time 2024 11 11)
                   :title "1 Corinthians 15:33"
                   :verse '(Corinthians1-15-33)
                   :image "/img/1-corinthians-15-33.jpg"
                   :audio-en "/audio/en/1-corinthians-15-33-en.m4a"
                   :audio-cn "/audio/cn/1-corinthians-15-33-cn.m4a"}

                  {:date (t/date-time 2024 11 12)
                   :title "Psalm 120:2"
                   :verse '(Psalm-120-2)
                   :image "/img/psalm-120-2.jpg"
                   :audio-en "/audio/en/psalm-120-2-en.m4a"
                   :audio-cn "/audio/cn/psalm-120-2-cn.m4a"}

                  {:date (t/date-time 2024 11 13)
                   :title "Deuteronomy 4:39"
                   :verse '(Deuteronomy-4-39)
                   :image "/img/deuteronomy-4-39.jpg"
                   :audio-en "/audio/en/deuteronomy-4-39-en.m4a"
                   :audio-cn "/audio/cn/deuteronomy-4-39-cn.m4a"}

                  {:date (t/date-time 2024 11 14)
                   :title "1 Peter 3:15"
                   ;; :verse '(CON '(REVERE) '(PREPARE) '(DO))
                   :verse '(CON '(REVERE1 nil '(CHRIST) :target '(LORD) :location '(IN-YOUR-HEART) :connect '(BUT))
                                '(PREPARE1 nil nil :target '(GIVE-ANSWER) :frequency '(ALWAYS))
                                '(DO-THIS nil nil :attitude '(GENTLENESS-AND-RESPECT)))
                   :image "/img/1-peter-3-15.jpg"
                   :audio-en "/audio/en/1-peter-3-15.m4a"
                   :audio-cn "/audio/cn/1-peter-3-15.m4a"}

                  {:date (t/date-time 2024 11 15)
                   :title "Proverbs 15:33"
                   :verse '(CON '(BE) '(COME))
                   :image "/img/proverbs-15-33.jpg"
                   :audio-en "/audio/en/proverbs-15-33-en.m4a"
                   :audio-cn "/audio/cn/proverbs-15-33-cn.m4a"}

                  {:date (t/date-time 2024 11 16)
                   :title "Luke 9:24"
                   :verse '(Luke-9-24)
                   :image "/img/luke-9-24.jpg"
                   :audio-en "/audio/en/luke-9-24-en.m4a"
                   :audio-cn "/audio/cn/luke-9-24-cn.m4a"}

                  {:date (t/date-time 2024 11 17)
                   :title "Titus 3:5"
                   :verse '(Titus-3-5)
                   :image "/img/titus-3-5.jpg"
                   :audio-en "/audio/en/titus-3-5-en.m4a"
                   :audio-cn "/audio/cn/titus-3-5-cn.m4a"}

                  {:date (t/date-time 2024 11 18)
                   :title "Nahum 1:3"
                   :verse '(Nahum-1-3)
                   :image "/img/nahum-1-3.jpg"
                   :audio-en "/audio/en/nahum-1-3-en.m4a"
                   :audio-cn "/audio/cn/nahum-1-3-cn.m4a"}

                  {:date (t/date-time 2024 11 19)
                   :title "Matthew 24:14"
                   :verse '(Matthew-24-14)
                   :image "/img/matthew-24-14.jpg"
                   :audio-en "/audio/en/matthew-24-14-en.m4a"
                   :audio-cn "/audio/cn/matthew-24-14-cn.m4a"}

                  ]
  )
;; (CREATE '(GOD) '(AND '(HEAVEN {:number "Plural" :spec "Definite"}) '(EARTH)) {:tense "Past" :time '(BEGIN)})

