(ns org.ego.xmpp
  (:gen-class)
  (:import [org.apache.commons.codec.binary Base64])
  (:require [org.ego.server :as server]
            [org.ego.db.accounts :as accounts]
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

(def #^{:private true
        :doc "Collections of IPs to streams"}
     xmpp-streams (ref {}))

(def #^{:private true
        :doc "Map of JIDs to streams"}
     jid-streams (ref {}))

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
                              
(defn online?
  [jid]
  (let [[user domain resource] (parse-jid jid)]
    (log :debug (str "IS " jid " ONLINE!??!"))
    (if (and (= domain (properties :server:domain))
             (@jid-streams [user domain]))
      true
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Channel Handler



(defmulti #^{:private true} process 
  "Takes msg strings and returns a vector of generated element structs"
  (fn [_ event & _]  event))

(defmethod process :connect
  [_ _ ip]
  (dosync (alter-nil xmpp-streams assoc ip (ref (assoc new-xmpp-stream :ip ip)))))
  
(defmethod process :disconnect
  [fun _ ip]
  (do (doseq [msg (fun :disconnect (@xmpp-streams ip))]
        (log :error msg)
        (let [jid (parse-jid (-> msg :attrs :to))
              ip (if (not (nil? (@jid-streams jid))) 
                   (:ip @(@jid-streams jid))
                   (if (not (nil? (@jid-streams (take 2 jid))))
                     (:ip @(@jid-streams (take 2 jid)))
                     nil))]
          (if (online? (-> msg :attrs :to))
            (do (log :error (str "YES " ip " : " msg))
                (server/channel-write ip msg)))))
      (dosync (alter-nil jid-streams dissoc (vector (:username @(@xmpp-streams ip)) (:server:domain properties)))
              (alter-nil xmpp-streams dissoc ip))))
              
    
(defmethod process :upstream
  [fun _ ip msg]
  (let [stream (@xmpp-streams ip)
        [user domain resource] (parse-jid (:to (:attrs msg)))]
    (log :debug (str "XMPP --> " msg))
    (let [return (fun msg stream)]
      (if (not (nil? (:username @stream)))
        (dosync (alter jid-streams assoc 
                       (vector (:username @stream) (:server:domain properties))
                       stream)))
      (doseq [m return]
        (let [jid (parse-jid (:to (:attrs m)))]
          (log :debug (str "XMPP <-- " (str m)))
          (server/channel-write (if (not (nil? (@jid-streams jid))) 
                                  (:ip @(@jid-streams jid))
                                  (if (not (nil? (@jid-streams (take 2 jid))))
                                    (:ip @(@jid-streams (take 2 jid)))
                                    ip))
                                  m))))))

(defmethod process :default [_ _ ip msg] msg)

(defn get-process 
  [fun]
  (partial process fun))