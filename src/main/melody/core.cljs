(ns melody.core
  (:require [rum.core :as rum]
            [melody.oss :as oss]
            [melody.idxdb :as idxdb]
            [cljs.core.async :as a]
            [cljs.core.async.interop :refer [p->c]]
            ["buffer" :refer [Buffer]]
            ["path" :as path]
            [cljs.spec.alpha :as s]
            ["crypto-js" :as c]))
;;; utils
(defn- ->array-buffer [o]
  (cond
    (instance? js/Uint8Array o)
    (.-buffer o)
    (instance? js/ArrayBuffer o)
    o
    :else
    (throw (js/Error. (type o)))))

;;; indexdb operations
(def db (atom nil))
(def store-name "melody-store")
(idxdb/create-db "melody" 1
                 #(idxdb/create-store % store-name {:keyPath "name"})
                 #(reset! db %))

(defn store-song! [name data]
  (idxdb/add-item @db store-name {:name name :data data} #(println (str "[" name "] stored"))))

(defn delete-song! [name]
  (idxdb/delete-item @db store-name name #(println "succ")))

(defn get-song-data [name]
  (a/go
    (let [ch (a/chan 1)]
      (idxdb/get-by-key @db store-name name #(let [data (:data %)]
                                               (when data
                                                 (a/offer! ch data))
                                               (a/close! ch)))
      (a/<! ch))))

;;; specs
(s/def ::display-type #{:personal-playlist
                        :all-songs-list
                        :upload})
(s/def ::play-list-mode #{:random
                          :ordered
                          :cycle})

(s/def ::source #(instance? js/AudioBufferSourceNode %))
(s/def ::gain-node #(instance? js/GainNode %))
(s/def ::audio-ctx #(instance? js/AudioContext %))
(s/def ::play-source (s/keys :req-un [::source ::gain-node ::audio-ctx]))

;;; state
(def state (atom { ;; which panel to display
                  :display-type :personal-playlist
                  :personal-playlist nil
                  ;; all songs list
                  :all-songs nil

                  :current-volume 0.4
                  :current-play-source nil
                  :current-play-song-name nil

                  ;; control chans
                  :pause (a/chan (a/dropping-buffer 1))
                  :skip (a/chan (a/dropping-buffer 1))
                  :replay (a/chan (a/dropping-buffer 1))

                  ;; play list
                  :play-list nil
                  :play-list-mode :ordered

                  :accesskey-id nil
                  :accesskey-secret nil}))

(defn change-default-volume! [v]
  (swap! state assoc :current-volume (/ v 100)))

;;; interop with oss
;; oss dirs:
;; - <bucket>/data/<song-name> -> data
;;   song data
;; - <bucket>/users/<accesskey-id>/<song-name> -> ""
;;   personal playlist

(def client ;; (oss/create-client accesskey-id accesskey-secret)
  nil)

(defn personal-playlist--get []
  (a/go
    (->>
     (a/<! (oss/list client :prefix (path/join "users" (:accesskey-id @state))))
     (map (comp js/decodeURIComponent path/basename :name)))))

(defn personal-playlist--add! [name]
  (oss/put client (path/join "users" (:accesskey-id @state) (js/encodeURIComponent name)) ""))

(defn personal-playlist--remove! [name]
  (oss/delete client (path/join "users" (:accesskey-id @state) (js/encodeURIComponent name))))

(defn get-all-songs
  []
  (a/go (sequence
         (comp
          (filter #(not= 0 (:size %)))
          (map (comp js/decodeURIComponent path/basename :name)))
         (a/<! (oss/list client :prefix (path/join "data"))))))

(defn download!
  "fetch from oss then store into indexdb"
  [name]
  (a/go (some->> (a/<! (oss/get client (path/join "data" (js/encodeURIComponent name))))
                 (store-song! name))))

(defn upload!
  "upload File obj to oss and store into indexdb"
  [name file]
  (a/go
    (println "start uploading" name)
    (let [arr-buf(a/<! (p->c (.arrayBuffer file)))
          buf (.from Buffer arr-buf)
          r (a/<! (oss/put client (path/join "data" (js/encodeURIComponent name)) buf))]
      (if (instance? ExceptionInfo r)
        (throw (js/Error. r))
        (store-song! name arr-buf)))))

;;; play
(defn audio-data->audio-buffer-source
  "return ::play-source
  https://developer.mozilla.org/en-US/docs/Web/API/BaseAudioContext/decodeAudioData
  https://developer.mozilla.org/en-US/docs/Web/API/BaseAudioContext/createGain"
  [data-buffer & {:keys [init-volume] :or {init-volume 1}}]
  (let [audio-ctx (new js/AudioContext)
        gain-node (.createGain audio-ctx)
        source (.createBufferSource audio-ctx)
        ch (a/chan 1)]
    (set! (-> gain-node .-gain .-value) init-volume)
    (.decodeAudioData audio-ctx (->array-buffer data-buffer)
                      (fn [buffer]
                        (set! (.-buffer source) buffer)
                        ;; (.connect source (.-destination audio-ctx))
                        (.connect source gain-node)
                        (.connect gain-node (.-destination audio-ctx))
                        (a/offer! ch true))
                      (fn [e] (println "error with decoding audio data" e)))
    (a/go (a/<! ch)
          (a/close! ch)
          (s/conform ::play-source
           {:source source
            :gain-node gain-node
            :audio-ctx audio-ctx}))))

(defn song-name->data
  [name]
  (a/go (or
         (a/<! (get-song-data name)) ;; search in indexdb first
         (a/<! (download! name))   ; if not exist, then download from oss
         (a/<! (get-song-data name)) ; finally fetch from indexdb again
         (throw (js/Error. (str "not found [" name "]")))))) ; still not found? throw error

(defn play!
  "return ::play-source
  close end-ch when onended"
  [name end-ch init-volume]
  (a/go
    (let [{:keys [source] :as res}
          (-> (a/<! (song-name->data name))
              (audio-data->audio-buffer-source :init-volume init-volume)
              a/<!)]
      (when end-ch (set! (.-onended source) #(a/close! end-ch)))
      (.start source)
      (s/conform ::play-source res))))

(defn stop!
  [{:keys [source] :as opts}]
  {:pre [(s/valid? ::play-source opts)]}
  (.stop source))

(defn change-volume!
  [play-source v]
  {:pre [(s/valid? ::play-source play-source)]}
  (set! (.-value (.-gain ^js/GainNode (:gain-node play-source))) (/ v 100)))


;;; different types of playlist
(defn random-playlist
  "return infinite random playlist"
  [candidates]
  {:pre [(seq candidates)]}
  (lazy-seq
   (cons (nth candidates (rand-int (count candidates)))
         (random-playlist candidates))))

(defn ordered-playlist
  "return infinite ordered-playlist"
  [candidates]
  {:pre [(seq candidates)]}
  (concat candidates (lazy-seq (ordered-playlist candidates))))

(defn cycle-playlist
  [item]
  (repeat item))

(defn update-playlist!
  [mode]
  (a/go
    (let [mode (s/conform ::play-list-mode (keyword mode))
          personal-playlist (:personal-playlist @state)
          playlist
          (case mode
            :random
            (random-playlist personal-playlist)
            :ordered
            (ordered-playlist personal-playlist)
            :cycle
            (cycle-playlist (:current-play-song-name @state)))]
      (swap! state assoc :play-list playlist)
      (swap! state assoc :play-list-mode mode))))



;;; play playlist
(defn play-playlist!
  "play songs on playlist,
  return rest playlist when pause-ch triggered"
  []
  (a/go
    ;; drain chans
    (a/poll! (:pause @state))
    (a/poll! (:skip @state))
    (a/poll! (:replay @state))
    (let [playlist (or (:play-list @state)
                       (do (a/<! (update-playlist! (:play-list-mode @state)))
                           (:play-list @state)))
          end-ch (a/chan)
          ;; play first song on playlist
          play-source (a/<! (play! (first playlist) end-ch (:current-volume @state)))
          _ (swap! state assoc :current-play-song-name (first playlist))
          _ (swap! state assoc :current-play-source play-source)
          {:keys [pause skip replay end]}
          {(a/alt!
             (:pause @state) ([_] (println :pause) :pause)
             (:skip @state) ([_] (println :skip) :skip)
             (:replay @state) ([_] (println :replay) :replay)
             end-ch ([_] (println :end) :end))
           true}
          playlist2 (:play-list @state)]
      (cond
        pause
        (do (stop! play-source)
            (rest playlist2))
        skip
        (do (stop! play-source)
            (swap! state assoc :play-list (rest playlist2))
            (a/<! (play-playlist!)))
        replay
        (do (stop! play-source)
            (swap! state assoc :play-list playlist2)
            (a/<! (play-playlist!)))
        end
        (do (swap! state assoc :play-list (rest playlist2))
            (a/<! (play-playlist!)))))))

(defn priority-play!
  [name]

)
;;; TODO: encrypt

;;; ui

(rum/defc refresh
  [atom]
  [:button {:type "button"
            :on-click #(swap! atom not)}
   "refresh"])

(rum/defc select-display-type < rum/reactive
  []
  (let [*display-type (rum/cursor state :display-type)
        display-type (rum/react *display-type)]
    [:div
     (keep (fn [s] (when (not= s display-type)
                     [:button
                      {:key s
                       :type "button"
                       :on-click #(reset! *display-type s)}
                      (name s)]))
           [:personal-playlist :all-songs-list :upload])]))

(def *refresh-all-songs-list (atom false))
(rum/defc all-songs-list < rum/reactive
  []
  (let [_ (rum/react *refresh-all-songs-list)
        *all-songs-list (rum/cursor state :all-songs)
        all-songs-list (rum/react *all-songs-list)]
    (a/go (->> (a/<! (get-all-songs)) (reset! *all-songs-list)))
    [:div
     [:ol
      (for [name all-songs-list]
        [:li {:key name}
         [:div
          [:div name]
          [:button {:type "button"
                    :on-click #(personal-playlist--add! name)}
           "add to playlist"]]])]]))

(def *refresh-personal-play-list (atom false))

(rum/defc personal-play-list < rum/reactive
  []
  (let [_ (rum/react *refresh-personal-play-list)
        *personal-play-list (rum/cursor state :personal-playlist)
        personal-play-list (rum/react *personal-play-list)]
    (a/go (->> (a/<! (personal-playlist--get)) (reset! *personal-play-list)))
    [:div
     (refresh *refresh-personal-play-list)
     [:ol
      (for [name personal-play-list]
        [:li {:key name}
         [:div
          [:div name]
          [:button {:type "button"
                    :on-click #(a/go
                                 (a/<! (personal-playlist--remove! name))
                                 (swap! *refresh-personal-play-list not))}
           "remove from playlist"]
          [:button {:type "button"
                    :on-click (fn [_]
                                (a/go
                                  (let [source (a/<! (play! name nil (:current-volume @state)))]
                                    (reset! (rum/cursor state :current-play-source) source)
                                    (swap! state assoc :current-play-song-name name))))}
           "play"]]])]]))

(rum/defc upload
  []
  (let [name (volatile! nil)
        file (volatile! nil)]
    [:div
     [:input {:type "text"
              :required true
              :on-input (fn [e] (vreset! name (-> e .-target .-value)))}]
     [:input {:type "file"
              :on-change (fn [e]
                           (vreset! file (-> e .-target .-files first)))}]
     [:button {:type "button"
               :on-click #(upload! @name @file)}
      "upload"]]))

(rum/defc playlist-mode < rum/reactive
  []
  (let [*mode (rum/cursor state :play-list-mode)
        mode (rum/react *mode)]
    [:select
     {:on-change #(update-playlist! (-> % .-target .-value))}
     [:option {:selected (and (= :random mode) "selected")} "random"]
     [:option {:selected (and (= :ordered mode) "selected")} "ordered"]
     [:option {:selected (and (= :cycle mode) "selected")} "cycle"]]))

(rum/defc controller < rum/reactive
  []
  (let [current-play-song-name (rum/react (rum/cursor state :current-play-song-name))]
    [:div
     (playlist-mode)
     (when-not current-play-song-name
       [:button {:type "button"
                 :on-click #(play-playlist!)}
        "play"])
     (when current-play-song-name
       [:button {:type "button"
                 :on-click #(a/offer! (:pause @state) true)}
        "pause"])
     (when current-play-song-name
       [:button {:type "button"
                 :on-click #(a/offer! (:skip @state) true)}
        "skip"])
     (when current-play-song-name
       [:button {:type "button"
                 :on-click #(a/offer! (:replay @state) true)}
        "replay"])
     [:input {:type "range"
              :on-change #(let [v (-> % .-target .-value js/parseInt)]
                            (some-> (:current-play-source @state)
                                    (change-volume! v))
                            (change-default-volume! v))}]]))

(rum/defc main-ui < rum/reactive
  []
  (let [*accesskey-id (rum/cursor state :accesskey-id)
        *accesskey-secret (rum/cursor state :accesskey-secret)
        accesskey-id (rum/react *accesskey-id)]
    (if (and accesskey-id client)
      (let [display-type (rum/react (rum/cursor state :display-type))]
        [:div
         ;; switch display type
         (select-display-type)
         [:hr]
         (controller)
         [:hr]
         (case display-type
           :personal-playlist
           (personal-play-list)
           :all-songs-list
           (all-songs-list)
           :upload
           (upload))])
      ;; login
      (let [id (volatile! nil)
            secret (volatile! nil)]
        [:div
         [:div
          [:label "ID:"]
          [:input {:type "text"
                   :on-change #(vreset! id (-> % .-target .-value))}]]
         [:div
          [:label "secret:"]
          [:input {:type "password"
                   :on-change #(vreset! secret (-> % .-target .-value))}]]
         [:button {:type "button"
                   :on-click #(do
                                (reset! *accesskey-id @id)
                                (reset! *accesskey-secret @secret)
                                (set! client (oss/create-client @id @secret)))}
          "submit"]]))))


(rum/mount (main-ui) (js/document.getElementById "root"))