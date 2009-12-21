(ns org.ego
  (:gen-class)
  (:require [org.ego.xmpp.xml :as xml]
            [org.ego.xmpp.server :as server]
            [org.ego.xmpp.stanza :as stanza]
            [org.ego.xmpp.xmpp :as xmpp]
            [org.ego.xmpp.stream :as stream])
  (:use [org.ego.core.common :only [properties]]
     ;   [org.ego.json.servlet :only [ego-servlet]]
        [clojure.contrib.logging :only [log]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Main

(def *server* (ref nil))

(defn -main
  [& args]
  (do (log :info "Starting XMPP Server on port 5222")
      (let [server (server/start-server 5222 xml/process stanza/process (xmpp/get-process stream/parse))]
        (dosync (ref-set *server* server)))
      (log :info "Starting JSON Server on port 8080")))
   ;   (run-server {:port 8080}
               ;   "/*"
                ;  (servlet ego-servlet))))
