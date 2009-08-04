(ns org.ego.xmpp
  (:gen-class)
  (:import [org.apache.commons.codec.binary Base64])
  (:require [org.ego.server :as server]
            [org.ego.db.accounts :as accounts]
            [org.ego.xmpp.iq :as iq]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Channel Handler

(def #^{:private true
        :doc "Collections of open XML element buffers"}
     xmpp-streams (ref {}))

(defmulti #^{:private true} process 
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
  (do (log :debug (str "XMPP --> " msg))
      (let [stream (@xmpp-streams ip)
            return (stream/parse msg stream)]
        (log :debug (str "XMPP <-- " return))
        (if (not (nil? return))
          (server/channel-write return))
        nil)))

(defmethod process :default [_ ip msg] msg)
