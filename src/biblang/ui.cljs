(ns biblang.ui
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [hnc-gen.bible :as bib]))

(defonce state (r/atom {}))

(defn button-clicked [_ev]
  (swap! state update-in [:number] inc))

(def tree-data1 '(root
                  (branch-1
                    (leaf-1a)
                    (leaf-1b))
                  (branch-2
                    (leaf-2a)
                    (leaf-2b)
                    (leaf-2c))))

(def tree-data2 bib/s1)

;; (def tree-data '(CREATE '(GOD) '(AND '(HEAVEN :number "Plural" :spec "Definite") '(EARTH)) :tense "Past" :time '(BEGIN)))
(def tree-data bib/s1)

(defn render-tree [node]
  [:ul
   [:li
    (if (sequential? node)
      [:span
       (name (first node))
       (for [child (rest node)]
         ^{:key (gensym)} [render-tree child])]
      (name node))]])

(defn top-division []
  [:div {:style {:width "97.5%"
                 :text-align "center"
                 :padding "10px"
                 :background-color "#d0e0f0"}}
   [:h2 "创世纪"]
   [:img {:src "https://img.heartlight.org/overlazy/creations/3748.jpg"
          :alt "Placeholder Image"
          :style {:max-width "100%"
                  :height "auto"}}]])

(defn left-division []
  (do
    ;; (println (bib/eval-str "(+ 1 2)"))
    (bib/English)
    [:div {:style {:width "47%"
                 :float "left"
                 :padding "10px"
                 :background-color "#f0f0f0"}}
   [:p (bib/sci-eval bib/s1)]]) ;; ((bib/BOOK-BIBLE) 0)]])

    )
   ;"This is the left division containing a sentence. afdafkdsafjdsafdjdsakfjdsakfjsdfjasdfjsdkfjsdkfjkdsfja"]])

(defn right-division []
  (do
    ;; (intern 'hnc-gen.bible 'language :Chinese)
    (bib/Chinese)
  [:div {:style {:width "47%"
                 :float "right"
                 :padding "10px"
                 :background-color "#e0e0e0"}}
      [:p (bib/sci-eval bib/s1)]]))

;;   [:p "This is the right division with another sentence."]]))

(defn bottom-division-1 []
  [:div {:style {:clear "both"
                 :padding "10px"
                 :background-color "#d0d0d0"}}

    [:p "This is the bottom division, containing the third sentence."]])

(defn bottom-division []
  [:div {:style {:clear "both"
                 :padding "10px"
                 :background-color "#d0d0d0"}}
   [:h2 "Tree Structure"]
   [render-tree tree-data]])


(defn main-page []
  [:div
   [top-division]
   [:div {:style {:overflow "hidden"}} ; This ensures that the float layout works correctly
   [left-division]
   [right-division]]
   [bottom-division]])

(defn component-main [state]
  [:div
   [:h1 "biblang"]
   [:p "Welcome to the app!"]
   [:button {:on-click button-clicked} "click me"]
   [:pre (pr-str @state)]
   [:p [:a {:href "/mypage"} "Static server rendered page."]]
   [:p [:a {:href "/api/example.json"} "JSON API example."]]])

(defn start1 {:dev/after-load true} []
  (rdom/render [component-main state]
               (js/document.getElementById "app")))

(defn start {:dev/after-load true} []
  (rdom/render [main-page]
               (js/document.getElementById "app")))

(defn main! []
  (start))
