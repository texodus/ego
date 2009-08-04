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

(defmulti process (fn [content _] [(-> content :content first :tag) 
                              (-> content :attrs :type keyword)
                              (-> content :content first :attrs :xmlns)]))

(defmethod process [:bind :set "urn:ietf:params:xml:ns:xmpp-bind"]
  [content state]
  (let [resource (gen-resource (-> content :content :content :content))]
    (do (log :debug (str "bound to resource " resource))
        (dosync (alter state assoc :resource resource))
        [{:tag :iq
          :attrs {:id (-> content :attrs :id)
                  :type "result"}
          :content [{:tag :bind
                     :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-bind"}
                     :content [{:tag :resource 
                                :content [resource]}]}]}])))
                           
(defmethod process [:session :set "urn:ietf:params:xml:ns:xmpp-session"]
  [content state]
  (do (log :debug "opened session")
      (dosync (alter state assoc :session true))
      [{:tag :iq
        :attrs {:id (-> content :attrs :id)
                :type "result"}
        :content [{:tag :session
                   :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-session"}}]}]))

(defmethod process :default
  [content state]
  (do (log :debug (str "sent unknown IQ " content))
      [{:tag :service-unavailable}]))
  

