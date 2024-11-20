(ns biblang.ui
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [hnc-gen.bible :as bib]
            [clojure.string :as str]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]))

(defonce state (r/atom {}))

(def max-page (count bib/daily-bread))

;; Define the app state
(def app-state (r/atom {:item nil
                        :error nil
                        :loading false}))


(def current-page (r/atom max-page))

;; Function to fetch item by number
(defn fetch-item [number]
  (swap! app-state assoc :loading true :error nil)
  (go
    (let [response (<! (http/get (str "/api/items/" number)
                                 {:with-credentials? false}))]
      (if (:success response)
        (swap! app-state assoc :item (:body response) :loading false)
        (swap! app-state assoc :error "Item not found" :loading false :item nil)))))

(defn highlight-phrase [sentence phrase]
  (if (str/blank? phrase)
    [:span sentence]
    (let [escaped-phrase (str/replace phrase #"[.*+?^${}()|[\\]\\\\]" "\\$&")
          pattern (re-pattern (str "(?i)(" escaped-phrase ")"))
          parts (str/split sentence pattern)]
      (into [:span]
            (mapcat (fn [part i]
                      (if (even? i)
                        [[:span part]]
                        [[:span {:style {:color "red"}} part]]))
                    parts
                    (range))))))

(defn highlight-phrase1 [sentence phrase]
  (if (str/blank? phrase)
    [:span sentence]
    (let [pattern (re-pattern (str "(?i)(" (str/replace phrase #"[.*+?^${}()|[\\]\\\\]" "\\$&") ")"))
          parts (str/split sentence pattern)]
      ;;(println parts)
      (into [:span]
            (interpose [:span {:style {:color "red"}} phrase]
                       (map (fn [part] [:span part]) parts))))))

(defn remove-quotes [x]
  (cond
    (sequential? x)
    (if (and (seq x) (= (first x) 'quote))
      (remove-quotes (second x))          ; Skip 'quote' and process the next element
      (map remove-quotes x))              ; Recursively process the list
    :else x))                             ; Return the element if it's not a list

(def responsive-styles
  {:content-container {:display "flex"
                       :flex-wrap "wrap"}
   :division {:padding "10px"}
   :left-division {:background-color "#f0f0f0"}
   :right-division {:background-color "#e0e0e0"}})

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

(defn verse [page]
  (:verse (bib/daily-bread (dec page))))

(defn title [page]
  (:title (bib/daily-bread (dec page))))

(defn image [page]
  (:image (bib/daily-bread (dec page))))

(defn audio-en [page]
  (:audio-en (bib/daily-bread (dec page))))

(defn audio-cn [page]
  (:audio-cn (bib/daily-bread (dec page))))

;; (def tree-data '(CREATE '(GOD) '(AND '(HEAVEN :number "Plural" :spec "Definite") '(EARTH)) :tense "Past" :time '(BEGIN)))
(def tree-data (verse 1))

(defn render-tree [node]
  [:ul
   [:li
    (if (sequential? node)
      [:span
       (name (first node))
       (for [child (rest node)]
         ^{:key (gensym)} [render-tree child])]
      (name node))]])

;; Define an atom to hold the selected node
(def selected-node (r/atom nil))

(defn check-item [item]
  (if (or (nil? item) (= item 'quote))
    false
    true))

(defn tree-component [lst]
  [:ul
   (for [item lst]
     ;; (when item  ; Ensure item is not nil
     (when (check-item item)
       ^{:key (hash item)}
       [:li
        {:on-click (fn [e]
                     (.stopPropagation e)      ; Prevent event bubbling
                     (reset! selected-node lst))}
        (if (sequential? item)
          [tree-component item]  ; Recursive rendering for nested lists
          (str item))]))])

(defn button-style [side]
  {:position "absolute"
   :top "10px"
   side "10px"
   :padding "10px 20px"
   :background-color "#4CAF50"
   :color "white"
   :border "none"
   :cursor "pointer"
   :border-radius "5px"})

(defn dec-rot [x]
  (inc (mod (- x 2) max-page))
  )

(defn inc-rot [x]
  (inc (mod x max-page))
  )

(defn prev-page [current-page]
  (swap! current-page dec-rot)
  ;; (fetch-item @current-page)
  )

(defn next-page [current-page]
  (swap! current-page inc-rot)
  ;; (fetch-item @current-page)
  )

(defn top-division []
 ;; (let [current-page (r/atom 1)]
    (fn []
      [:div {:style {:width "97.5%"
                     :text-align "center"
                     :padding "10px"
                     :background-color "#d0e0f0"
                     :position "relative"}}
       [:button {:style (button-style "left")
                 ;; :on-click #(swap! current-page dec)
                 :on-click #(prev-page current-page)
                 ;; :disabled (= @current-page 1)
                 }
        "Previous"]
       [:h2 (title @current-page)]
       ;; [:img {:src (str "https://via.placeholder.com/400x200?text=Page+" @current-page)
       ;; [:img {:src (str "https://img.heartlight.org/overlazy/creations/3748.jpg?text=Page+" @current-page)
       [:img {:src (image @current-page)

              :alt (str "Page " @current-page)
              :style {:max-width "100%"
                      :height "auto"}}]
       [:button {:style (button-style "right")
                 ;; :on-click #(swap! current-page inc)
                 :on-click #(next-page current-page)
                 ;; :disabled (= @current-page max-page)
                 }
        "Next"]]))

(defn top-division1 []
  [:div {:style {:width "97.5%"
                 :text-align "center"
                 :padding "10px"
                 :background-color "#d0e0f0"}}
   [:h2 "创世纪"]
   [:img {:src (image @current-page)
          :alt "Placeholder Image"
          :style {:max-width "100%"
                  :height "auto"}}]])

(defn play-audio [audio-src]
  (let [audio (js/Audio. audio-src)]
    (.play audio)))

(defn left-division []
;;  (let [audio-src "file:///e:/usr/forrest/audio.mp3"]
  (let [audio-src (audio-en @current-page)] ; Replace with your actual audio file path
    (bib/English)
    [:div {:class "division left-division"
           :style (merge (:division responsive-styles)
                         (:left-division responsive-styles))}
     [:div {:style {:display "flex"
                    :align-items "center"}}
      [:button {:on-click #(play-audio audio-src)
                :style {:background "none"
                        :border "none"
                        :cursor "pointer"
                        :margin-right "10px"}}
       [:svg {:xmlns "http://www.w3.org/2000/svg"
              :viewBox "0 0 24 24"
              :width "24"
              :height "24"}
        [:path {:fill "currentColor"
                :d "M8 5v14l11-7z"}]]]
     ;; [:p "This is the left division containing a sentence."]]]))
      [:p [highlight-phrase (bib/sci-eval (verse @current-page)) (bib/sci-eval @selected-node)]]
      ;;[:p (bib/sci-eval bib/s1)]
      ;;[:p (bib/sci-eval @selected-node)]
      ]])) ;; ((bib/BOOK-BIBLE) 0)]])

(defn right-division []
;;  (let [audio-src "file:///e:/usr/forrest/audio.mp3"]
  (let [audio-src (audio-cn @current-page)] ; Replace with your actual audio file path
    (bib/Chinese)
    [:div {:class "division right-division"
           :style (merge (:division responsive-styles)
                         (:right-division responsive-styles))}
     [:div {:style {:display "flex"
                    :align-items "center"}}
      [:button {:on-click #(play-audio audio-src)
                :style {:background "none"
                        :border "none"
                        :cursor "pointer"
                        :margin-right "10px"}}
       [:svg {:xmlns "http://www.w3.org/2000/svg"
              :viewBox "0 0 24 24"
              :width "24"
              :height "24"}
        [:path {:fill "currentColor"
                :d "M8 5v14l11-7z"}]]]
     ;; [:p "This is the left division containing a sentence."]]]))
      [:p [highlight-phrase (bib/sci-eval (verse @current-page)) (bib/sci-eval @selected-node)]]
      ;;[:p (bib/sci-eval bib/s1)]
      ;;[:p (bib/sci-eval @selected-node)]
      ]])) ;; ((bib/BOOK-BIBLE) 0)]])

(defn right-division1 []
  (bib/Chinese)
  [:div {:class "division right-division"
         :style (merge (:division responsive-styles)
                       (:right-division responsive-styles))}
  ;; [:p "This is the right division with another sentence."]])
   ;;   [:p (bib/sci-eval bib/s1)]
   [:p [highlight-phrase (bib/sci-eval (verse @current-page)) (bib/sci-eval @selected-node)]]
   ])

(defn bottom-division []
  [:div {:style {:clear "both"
                 :padding "10px"
                 :background-color "#d0d0d0"}}
   [:h2 "Sentence Analysis / 語句分析"]
   ;;[render-tree (remove-quotes tree-data)]])
  [tree-component (verse @current-page)]])

;; Component to display the item or error
(defn item-display []
  (let [{:keys [item error loading]} @app-state]
    [:div
     (cond
       loading [:p "Loading..."]
       error [:p.error error]
       item [:div
             [:h2 "Item Details"]
             [:p "Number: " (:number item)]
             [:p "Title: " (:title item)]
             ;; Add more item details as needed
             ]
       :else [:p "No item selected"])]))

(defn main-page []
  [:div
   [top-division]
   [:div {:class "content-container"
          :style (:content-container responsive-styles)}
    [left-division]
    [right-division]]
   [bottom-division]
   ;;[item-display]
   ])

(defn apply-media-queries []
  (let [style-el (js/document.createElement "style")]
    (set! (.-innerHTML style-el)
          (str "
               .content-container { display: flex; flex-direction: column; }
               .division { width: 100%; }
               @media (min-width: 768px) {
                 .content-container { flex-direction: row; }
                 .division { width: 47.5%; }
               }
               "))
    (.appendChild js/document.head style-el)))

(defn component-main [state]
  [:div
   [:h1 "biblang"]
   [:p "Welcome to the app!"]
   [:button {:on-click button-clicked} "click me"]
   [:pre (pr-str @state)]
   [:p [:a {:href "/mypage"} "Static server rendered page."]]
   [:p [:a {:href "/api/example.json"} "JSON API example."]]])

(defn start
  {:dev/after-load true}
  []
  ;; (fetch-item 1)
  (rdom/render [main-page]
               (js/document.getElementById "app"))
  (apply-media-queries))

(defn main! []
  (start))
