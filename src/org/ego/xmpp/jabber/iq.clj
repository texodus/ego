(ns org.ego.xmpp.jabber.iq
  (:gen-class)
  (:require [org.ego.core.common :as common]
            [org.ego.xmpp :as server]
            [org.ego.core.db.accounts :as accounts])
  (:use [org.ego.core.common :only [properties gen-id log]]))
 

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
    (do (log :info (str "bound to resource " resource))
        (dosync (alter state assoc :resource resource))
        [{:tag :iq
          :attrs {:id (-> content :attrs :id)
                  :type "result"}
          :content [{:tag :bind
                     :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-bind"}
                     :content [{:tag :jid 
                                :content [(str (:username @state) "@" (:server:domain properties) "/" (:resource @state))]}]}]}])))
                           
(defmethod process [:session :set "urn:ietf:params:xml:ns:xmpp-session"]
  [content state]
  (do (log :debug "opened session")
      (dosync (alter state assoc :session true))
      [{:tag :iq
        :attrs {:id (-> content :attrs :id)
                :to (str (:username @state) "@" (:server:domain properties) "/" (:resource @state))
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
               :attrs {:xmlns "http://jabber.org/protocol/disco#items"
                       :node "http://jabber.org/protocol/commands"}}]}])
               
(defmethod process [:query :get "http://jabber.org/protocol/disco#info"]
  [content state]
  [{:tag :iq
    :attrs {:from (:server:domain properties)
            :id (-> content :attrs :id)
            :to (str (:username @state) "@" (:server:domain properties) "/" (:resource @state))
            :type "result"}
    :content [{:tag :query
               :attrs {:xmlns "http://jabber.org/protocol/disco#info"}
               :content [{:tag :identity
                          :attrs {:category "server"
                                  :name "Ego Server"
                                  :type "im"}}
                         ;; {:tag :identity
                         ;;  :attrs {:category "component"
                         ;;          :type "presence"}}
                         {:tag :feature
                          :attrs {:var "jabber:iq:roster"}}
                         {:tag :feature
                          :attrs {:var "vcard-temp"}}]}]}])

(defmethod process [:query :get "jabber:iq:roster"]
  [content state]
  (let [friends (accounts/get-friends (:user-id @state))]
     (do (log :debug (str "requested roster [" (apply str (interpose ", " friends)) "]")) 
        [{:tag :iq
          :attrs {:id (-> content :attrs :id)
                  :to (str (:username @state) "@" (:server:domain properties) "/" (:resource @state))
                  :type "result"}
          :content [{:tag :query
                     :attrs {:xmlns "jabber:iq:roster"}
                     :content (for [friend friends]
                                {:tag :item
                                 :attrs {:jid friend
                                         :name friend
                                         :subscription "none"}})}]}])))

(defmethod process [:vCard :get "vcard-temp"]
  [content state]
 ; (let [friends (accounts/get-friends (:user-id @state))]
    (do (log :debug "requested vcard")
        [{:tag :iq
          :attrs {:from (:server:domain properties)
                  :id (-> content :attrs :id)
                  :to (str (:username @state) "@" (:server:domain properties) "/" (:resource @state))
                  :type "result"}
          :content [{:tag :vCard
                     :attrs {:xmlns "vcard-temp"}
                     :content ["THIS IS A TEMP vCard"]}]}]))

(defmethod process [:vCard :set "vcard-temp"]
  [content state]
 ; (let [friends (accounts/get-friends (:user-id @state))]
    (do (log :debug "set vcard")
        [{:tag :iq
          :attrs {:from (:server:domain properties)
                  :id (-> content :attrs :id)
                  :to (str (:username @state) "@" (:server:domain properties) "/" (:resource @state))
                  :type "result"}}]))
          

(defmethod process [:ping :get "urn:xmpp:ping"]
  [content state]
  (do (log :debug "ping!")
      [{:tag :iq
        :attrs {:id (-> content :attrs :id) 
                :from (:server:domain properties)
                :type "result"}}]))

(defmethod process [:query :get "http://jabber.org/protocol/bytestreams"]
  [content state]
  (do (log :debug "Requested bytestreams")
      [{:tag :iq
        :attrs {:from "proxy.jabber.org" ;(:server:domain properties)
                :id (-> content :attrs :id)
                :to (str (:username @state) "@" (:server:domain properties) "/" (:resource @state))
                :type "result"}
        :content [{:tag :query
                   :attrs {:xmlns "http://jabber.org/protocol/bytestreams"}
                   :content [{:tag :streamhost 
                              :attrs {:jid (:server:domain properties)
                                      :host "127.0.0.1"
                                      :port "7777"}}]}]}]))

(defmethod process :default
  [content state]
  (do (log :info (str "sent unknown IQ " content))
      [{:tag :iq
        :attrs {:id (-> content :attrs :id)
                :to (str (:username @state) "@" (:server:domain properties) "/" (:resource @state))
                :type "error"}
        :content [{:tag :error
                   :attrs {:code "501"
                           :type "cancel"}
                   :content [{:tag :feature-not-implemented
                              :attrs {:xmlns "urn.ietf.params.xml.ns.xmpp-stanzas"}}]}]}]))
  

