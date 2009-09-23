(ns org.ego
  (:gen-class)
  (:require [org.ego.xmpp.xml :as xml]
            [org.ego.xmpp.server :as server]
            [org.ego.xmpp.stanza :as stanza]
            [org.ego.xmpp.xmpp :as xmpp]
            [org.ego.xmpp.stream :as stream])
  (:use [org.ego.core.common :only [properties]]
        [clojure.contrib.logging :only [log]]))

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
      (let [server (server/start-server 5222 xml/process stanza/process (xmpp/get-process stream/parse))]
        (dosync (ref-set *server* server)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; REPL

;(defn start-server 
;  [] 
;  (let [server (server/create-server 5222 #(xml/parse xmpp/process xmpp/new-stream-state))]
;    (dosync (ref-set *server* server))))