(ns hnc-gen.core
  (:require
    [hnc-gen.term :as term]
    [hnc-gen.bible :as bible]
  )
)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn is-single? [lst]
  (if (and (seq lst) (= (first lst) 'GOD))
    true
    false))

(defn CREATE
  ([] (vector "create"))
;  ([{:keys [language]}] (if (= language "Chinese") (vector "创造") (vector "create")))
  ([creator object] (flatten (vector (eval creator) (if (is-single? creator) "creates" "create") (eval object)))))
;  ([creator object & {:keys [time tense]}]
;    (if (= tense :single)
;      (flatten (vector time "," creator "creates" object))
;      (flatten (vector time "," creator "create" object)))))

(defn GOD
  ([] (vector "God")))

(defn BEGIN
  ([] (vector "begin"))
  ([x] (if (= x :time) (vector "in the beginning") (vector "begin"))))

(defn HEAVEN
  ([] (vector "heaven")))

(defn EARTH
  ([] (vector "earth")))

(defn AND
  ([x,y] (flatten (vector (eval x) "and" (eval y)))))