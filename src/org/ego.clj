(ns org.ego
  (:gen-class)
  (:require [org.ego.xml :as xml]
            [org.ego.server :as server]
            [org.ego.stanza :as stanza])
  (:use [org.ego.common :only [properties log]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

;(def #^{:private true} conf (common/get-properties "server"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Main

(def *server* (ref nil))

(defmulti process 
  "Takes msg strings and returns a vector of generated element structs"
  (fn [event & _]  event))

(defmethod process :upstream
  [_ ip msg]
  (log :info msg))
  

(defmethod process :default [_ & _] nil)

(defn -main
  [& args]
  (do (log :info "Starting XMPP Server on port 5222")
      (let [server (server/start-server 5222 xml/process stanza/process process)]
        (dosync (ref-set *server* server)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; REPL

;(defn start-server 
;  [] 
;  (let [server (server/create-server 5222 #(xml/parse xmpp/process xmpp/new-stream-state))]
;    (dosync (ref-set *server* server))))