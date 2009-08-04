(ns org.ego.xmpp
  (:gen-class)
  (:import [org.apache.commons.codec.binary Base64])
  (:require [org.ego.server :as server]
            [org.ego.db.accounts :as accounts]
            [org.ego.xmpp.stream :as stream]
            [org.ego.common :as common])
  (:use [org.ego.common :only [properties gen-id]]
        [org.ego.server :only [log]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(defmacro alter-nil
  "Works like alter but returns nil"
  [& xs]
  `(do (alter ~@xs) nil))

(defstruct xmpp-stream :open :ssl :id :resource :session :username :user-id :ip)
(def new-xmpp-stream (struct xmpp-stream false false nil nil true nil nil nil))

(defn parse-jid
  [string]
  (if (empty? string)
    (vector nil nil nil)
    (if (. string (contains "@"))
    (let [parts (. string (split "@"))]
      (if (. (second parts) (contains "/"))
        (cons (first parts) (. (second parts) (split "/")))
        (vector (first parts) (second parts) nil)))
    (vector nil string nil))))
                              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Channel Handler

(def #^{:private true
        :doc "Collections of open XML element buffers"}
     xmpp-streams (ref {}))

(defmulti process 
  "Takes msg strings and returns a vector of generated element structs"
  (fn [event & _]  event))

(defmethod process :connect
  [_ ip]
  (dosync (alter-nil xmpp-streams assoc ip (ref (assoc new-xmpp-stream :ip ip)))))
  
(defmethod process :disconnect
  [_ ip]
  (dosync (alter-nil xmpp-streams dissoc ip)))

(defmethod process :upstream
  [_ ip msg]
  (let [[user domain resource] (parse-jid (:to (:attrs msg)))]
    (log :debug (str "# " user ":" domain ":" resource))
    (log :debug (str "XMPP --> " msg))
    (let [return (stream/parse msg (@xmpp-streams ip))]
      (log :debug (str "XMPP <-- " return))
        (if (not (nil? return))
          (server/channel-write return))
        nil)))

(defmethod process :default [_ ip msg] msg)
