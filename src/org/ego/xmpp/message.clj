(ns org.ego.xmpp.message
  (:gen-class)
  (:require [org.ego.common :as common]
            [org.ego.server :as server]
            [org.ego.db.accounts :as accounts])
  (:use [org.ego.common :only [properties gen-id]]
        [org.ego.server :only [log]]))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(defn gen-resource
  [string]
  (if (nil? string)
    (str "Home_" (gen-id))
    (str string "_" (gen-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Process

(defmulti process (fn [content _] (-> content :attrs :type keyword)))

; TODO make sure this user is allowed to receive this message
(defmethod process :chat
  [content state]
   (do (log :info (str "says to " (:to (:attrs content)) " : " (-> content :content first :content first)))
       [(assoc-in (assoc-in content [:attrs :id] (gen-id))
                  [:attrs :from] (str (:username @state) "@" (properties :server:domain)))]))

(defmethod process :default
  [content state]
  (do (log :debug (str "sent unknown IQ " content))
      [{:tag :service-unavailable}]))
  

