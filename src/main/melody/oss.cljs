(ns melody.oss
  (:refer-clojure :exclude [get list])
  (:require ["ali-oss" :as oss]
            ["buffer" :refer [Buffer]]
            [cljs.core.async :as a]
            [cljs.core.async.interop :refer [p->c]]))

(defn get
  "return buffer data"
  [client key]
  (a/go
    (-> (a/<! (p->c (.get client key)))
        (js->clj :keywordize-keys true)
        :res
        :data)))

(defn list
  "opts:
  - :max-keys int
  - :prefix   string
  - :delimiter string
  return object-meta list
  "
  ([client] (list client :max-keys 1000))
  ([client & opts]
   (let [{:as opts} opts]
     (a/go
       (->
        (a/<! (p->c (.listV2 client (clj->js opts))))
        (js->clj :keywordize-keys true)
        :objects)))))

(defn put
  "https://github.com/ali-sdk/ali-oss#putname-file-options"
  [client key data]
  (let [data (if (string? data) (Buffer data) data)]
    (p->c (.put client key data))))

(defn delete
  [client key]
  (p->c (.delete client key)))

(defn access-key-id
  [client]
  (:accessKeyId (js->clj (.-options client))))

(defn create-client [accesskey-id accesskey-secret]
  (oss (clj->js {:region "oss-cn-hangzhou"
                 :accessKeyId accesskey-id
                 :accessKeySecret accesskey-secret
                 :bucket "airport-in-10-30"})))
