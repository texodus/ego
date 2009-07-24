(ns org.ego
  (:gen-class)
  (:import [org.apache.log4j Logger])
  (:require [org.ego.common :as common]
            [org.ego.server :as server]
            [org.ego.xmpp :as xmpp]
            [org.ego.xml :as xml]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(def #^{:private true} conf (common/get-properties "server"))
(def #^{:private true} log (. Logger (getLogger (str *ns*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Main

(def *server* (ref nil))

(defn -main
  [& args]
  (do (. log (info "Starting XMPP Server on port 5222"))
      (server/create-server 5222 #(xml/parse xmpp/process xmpp/new-stream-state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; REPL

(defn start-server 
  [] 
  (let [server (server/create-server 5222 #(xml/parse xmpp/process xmpp/new-stream-state))]
    (dosync (ref-set *server* server))))