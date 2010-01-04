(ns org.ego.core
  (:gen-class)
  (:import [java.io FileReader BufferedReader BufferedInputStream BufferedOutputStream FileOutputStream InputStreamReader FileInputStream])
  (:require [clojure.contrib.logging :as logging]
            [org.ego.xmpp :as server]))

(def msg-queue (agent nil))

(defn queue 
  [ip msg]
  (send-off msg-queue (fn [_] (server/channel-write ip msg))))
