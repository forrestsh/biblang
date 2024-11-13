(ns biblang.server
  (:require
    [applied-science.js-interop :as j]
    [promesa.core :as p]
    ["fs" :as fs]
    ["source-map-support" :as sourcemaps]
    [sitefox.html :refer [render-into]]
    [sitefox.web :as web]
    [sitefox.db :as db]
    [sitefox.util :refer [env]]
    [sitefox.logging :refer [bind-console-to-file]]
    [sitefox.tracebacks :refer [install-traceback-handler]]
    [clojure.string :as str]
    [sitefox.deps :as deps]
    ["cors" :as cors]))  ; Add this line to require the CORS middleware

(bind-console-to-file)
(sourcemaps/install)
(let [admin-email (env "ADMIN_EMAIL")
      build-id (try (fs/readFileSync "build-id.txt") (catch :default _e "dev"))]
  (when admin-email
    (install-traceback-handler admin-email build-id)))

(defonce server (atom nil))

(def template (fs/readFileSync "public/index.html"))

;; Initialize the key-value store
(def data-table (db/kv "data-table"))

(defn my-page []
  [:main
   [:h1 "My page"]
   [:p "This is a server rendered page."]
   [:p [:a {:href "/"} "Return to the app"]]])

;; In-memory data store
(defonce data-store (atom []))

;; Helper function to generate a unique ID
(defn generate-id []
  (str (random-uuid)))

;; API endpoints for managing the table
(defn get-all-items [_req res]
  (.json res (clj->js @data-store)))

(defn get-item-by-id [req res]
  (let [id (j/get-in req [:params :id])
        item (first (filter #(= (:id %) id) @data-store))]
    (if item
      (.json res (clj->js item))
      (.status res 404))))

(defn create-item [req res]
  (let [body (js->clj (j/get req :body) :keywordize-keys true)
        new-item (assoc body :id (generate-id))]
    (swap! data-store conj new-item)
    (.status res 201)
    (.json res (clj->js new-item))))

(defn update-item [req res]
  (let [id (j/get-in req [:params :id])
        body (js->clj (j/get req :body) :keywordize-keys true)
        updated-store (mapv #(if (= (:id %) id) (merge % body) %) @data-store)]
    (if (not= @data-store updated-store)
      (do
        (reset! data-store updated-store)
        (.json res (clj->js (first (filter #(= (:id %) id) updated-store)))))
      (.status res 404))))

(defn delete-item [req res]
  (let [id (j/get-in req [:params :id])
        updated-store (vec (remove #(= (:id %) id) @data-store))]
    (if (not= (count @data-store) (count updated-store))
      (do
        (reset! data-store updated-store)
        (j/call res :sendStatus 204))
      (j/call res :status 404))))

(defn api-example [_req res]
  (.json res (clj->js {:question 42})))

(defn setup-routes [app]
  (web/reset-routes app)

  ;; Add CORS middleware
  ;; (j/call app :use (cors #js {:origin "http://localhost:8000"  ; Replace with your frontend URL
  ;;                            :credentials true}))
  ;; (j/call app :use (cors #js {:origin "*"}))

  ;; Add CORS middleware
  ;; (let [cors-options #js {:origin "http://localhost:8000"  ; Replace with your frontend URL
  ;;                        :credentials true}]
  ;;  (j/call app :use (cors cors-options)))

  ;; Add CSRF protection
  ;; (deps/csrf app)
  ;; (deps/csrf app #js {:ignoreMethods #js ["POST" "PUT" "DELETE"]})

  ;; Add a route to get the CSRF token
  ;; (j/call app :get "/csrf-token" (fn [req res]
  ;;  (.json res #js {:csrfToken (j/get req :csrfToken)})))

  (j/call app :get "/mypage" #(.send %2 (render-into template "body" [my-page])))
  (j/call app :get "/api/example.json" api-example)
  ;; (j/call app :post "/items" create-item)

  ;; New API routes
  (j/call app :get "/api/items" get-all-items)
  (j/call app :get "/api/items/:id" get-item-by-id)
  (j/call (j/get app "pre-csrf-router") :post  "/api/items" create-item)
  (j/call app :put "/api/items/:id" update-item)
  (j/call app :delete "/api/items/:id" delete-item)

  (web/static-folder app "/" "public")

  )

(defn main! []
  (p/let [[app host port] (web/start)]
    (reset! server app)
    (setup-routes app)
    (println "Serving on" (str "http://" host ":" port))))

(defn ^:dev/after-load reload []
  (js/console.log "Reloading.")
  (setup-routes @server))
