(ns org.ego.application
  (:gen-class)
  (:import [org.apache.log4j Logger])
  (:require [org.ego.config :as config]
            [org.ego.server :as server]
            [org.ego.xmpp :as xmpp]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(def #^{:private true} conf (config/get-properties "server"))
(def #^{:private true} log (. Logger (getLogger (str *ns*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Main

(def *server* (ref nil))

(defn -main
  [& args]
  (do (. log (info "Starting XMPP Server on port 5222"))
      (server/create-server 5222 xmpp/get-xmpp-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; REPL

(defn start-server 
  [] 
  (let [server (server/create-server 5222 xmpp/get-xmpp-parser)]
    (dosync (ref-set *server* server))))