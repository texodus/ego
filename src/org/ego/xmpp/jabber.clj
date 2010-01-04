(ns org.ego.xmpp.jabber
  (:gen-class)
  (:import [org.apache.commons.codec.binary Base64])
  (:require [org.ego.xmpp :as server]
            [org.ego.core.db.accounts :as accounts]
            [org.ego.core :as core]
            [org.ego.common :as common]
            [org.ego.xmpp.jabber.stream :as stream]
            [clojure.contrib.logging :as logging])
  (:use [org.ego.common :only [properties gen-id alter-nil parse-jid log]]))
        


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(deftype Jabber-stream [open ssl id resource session username user-id ip] clojure.lang.IPersistentMap)

(def new-jabber-stream (Jabber-stream false false nil nil true nil nil nil))

(def #^{:private true
        :doc "Collections of IPs to streams"}
     jabber-streams (ref {}))

(def #^{:private true
        :doc "Map of JIDs to streams"}
     jid-streams (ref {}))


(defn log-ip
  [ip & args]
  (let [output (if (nil? (:username @(@jabber-streams ip)))
                 (str ip " " (apply str (rest args)))
                 (if (nil? (:resource @(@jabber-streams ip)))
                   (str (:username @(@jabber-streams ip)) " " (apply str (rest args)))
                   (str (:username @(@jabber-streams ip)) "/" (:resource @(@jabber-streams ip)) " " (apply str (rest args)))))]
    (logging/log (first args) output)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Server implementation

(deftype Jabber []
  server/Server (connect [ip] (dosync (alter-nil jabber-streams assoc ip (ref (assoc new-jabber-stream :ip ip)))))
                (disconnect [jip] (binding [log (partial log-ip jip)]
                                   (do (doseq [msg (stream/parse :disconnect (@jabber-streams jip))]
                                         (let [jid (parse-jid (-> msg :attrs :to))
                                               ip (if (not (nil? (@jid-streams jid)))
                                                    (:ip @(@jid-streams jid))
                                                    (if (not (nil? (@jid-streams (take 2 jid))))
                                                      (:ip @(@jid-streams (take 2 jid)))
                                                      nil))]
                                          ;; (if (online? (-> msg :attrs :to)) TODO this logic should be handled in the downstream routing
                                             (server/channel-write ip msg)))
                                       (dosync (alter-nil jid-streams dissoc (vector (:username @(@jabber-streams jip)) (:server:domain properties)))
                                               (alter-nil jabber-streams dissoc jip)))))
                (upstream [ip msg] (binding [log (partial log-ip ip)]
                                     (let [stream (@jabber-streams ip)
                                           [user domain resource] (parse-jid (:to (:attrs msg)))]
                                       (log :debug (str "XMPP --> " (into {} msg)))
                                       (let [return (stream/parse msg stream)]
                                         (if (not (nil? (:username @stream)))
                                           (dosync (alter jid-streams assoc 
                                                          (vector (:username @stream) (:server:domain properties))
                                                          stream)))
                                         (doseq [m return] (core/queue ip m))))))
                (downstream [ip msg] (binding [log (partial log-ip ip)]
                                     (let [stream (@jabber-streams ip)
                                           [user domain resource] (parse-jid (:to (:attrs msg)))]
                                       (log :debug (str "XMPP <-- " msg))
                                       msg))))
                                       ;;(stream/emit msg stream)))))
