(ns org.ego.xmpp.iq
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

(defmethod process [:query :get "http://jabber.org/protocol/disco#items"]
  [content state]
  [{:tag :iq
    :attrs {:from (:server:domain properties)
            :id (-> content :attrs :id)
            :to (str (:username @state) "@" (:server:domain properties) "/" (:resource @state))
            :type "result"}
    :content [{:tag :query
               :attrs {:xmlns "http://jabber.org/protocol/disco#items"}}]}])

(defmethod process [:query :get "http://jabber.org/protocol/disco#info"]
  [content state]
  [{:tag :iq
    :attrs {:from (:server:domain properties)
            :id (-> content :attrs :id)
            :to (str (:username @state) "@" (:server:domain properties) "/" (:resource @state))
            :type "result"}
    :content [{:tag :query
               :attrs {:xmlns "http://jabber.org/protocol/disco#info"}
               :content [{:tag :feature
                          :attrs {:var "jabber:iq:roster"}}
                         {:tag :feature
                          :attrs {:var "vcard-temp"}}]}]}])

(defmethod process [:query :get "jabber:iq:roster"]
  [content state]
  (let [friends (accounts/get-friends (:user-id @state))]
     (do (log :debug (str "requested roster [" (apply str (interpose ", " friends)) "]")) 
        [{:tag :iq
          :attrs {:from (:server:domain properties)
                  :id (-> content :attrs :id)
                  :to (str (:username state) "@" (:server:domain properties) "/" (:resource @state))
                  :type "result"}
          :content [{:tag :query
                     :attrs {:xmlns "http://jabber.org/protocol/disco#info"}
                     :content (for [friend friends]
                                {:tag :item
                                 :attrs {:jid friend}})}]}])))

(defmethod process [:vCard :get "vcard-temp"]
  [content state]
 ; (let [friends (accounts/get-friends (:user-id @state))]
    (do (log :debug "requested vcard")
        [{:tag :iq
          :attrs {:from (:server:domain properties)
                  :id (-> content :attrs :id)
                  :to (str (:username state) "@" (:server:domain properties) "/" (:resource @state))
                  :type "result"}
          :content [{:tag :vCard
                     :attrs {:xmlns "vcard-temp"}
                     :content ["THIS IS A TEMP vCard"]}]}]))

(defmethod process [:ping :get "urn:xmpp:ping"]
  [content state]
  (do (log :debug "ping!")
      [{:tag :iq
        :attrs {:id (-> content :attrs :id) 
                :from (:server:domain properties)
                :type "result"}}]))

(defmethod process :default
  [content state]
  (do (log :debug (str "sent unknown IQ " content))
      [{:tag :service-unavailable}]))
  

