(ns org.ego.xmpp.iq
  (:gen-class)
  (:import [java.net ServerSocket Socket SocketException InetAddress InetSocketAddress URL]
           [java.io InputStreamReader OutputStreamWriter PushbackReader ByteArrayInputStream Reader Writer OutputStream FileInputStream]
           [javax.xml.parsers SAXParserFactory]
           [org.xml.sax.helpers DefaultHandler]
           [org.xml.sax InputSource]
           [org.apache.log4j Logger]
           [org.apache.commons.codec.binary Base64]
           [clojure.lang LineNumberingPushbackReader])
  (:require [org.ego.config :as config]
            [org.ego.xml :as xml]
            [org.ego.server :as server]
            [org.ego.db.accounts :as accounts]))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(def #^{:private true} log (. Logger (getLogger (str *ns*))))
(def #^{:private true} conf (config/get-properties "server"))

(defn- xmpplog
  [& string]
  (. log (info (str "IP " (server/get-ip) " " (apply str string)))))

(def id-counter (ref 1))

; TODO make an unpredictable (nonsequential) id generator
(defn gen-id
  []
  (do (dosync (alter id-counter inc))
      @id-counter))

(defn gen-resource
  [string]
  (if (nil? string)
    (str "Home_" (gen-id))
    (str string "_" (gen-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; XMPP : iq

(defmulti iq (fn [content _] [(-> content :content first :tag) 
                              (-> content :attrs :type keyword)
                              (-> content :content first :attrs :xmlns)]))

(defmethod iq [:bind :set "urn:ietf:params:xml:ns:xmpp-bind"]
  [content state]
  (let [resource (gen-resource (-> content :content :content :content))]
    (do (xmpplog "assigned resource " resource)
        (xml/emit-element {:tag :iq
                           :attrs {:id (-> content :attrs :id)
                                   :type "result"}
                           :content [{:tag :bind
                                      :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-bind"}
                                      :content [{:tag :resource 
                                                 :content [resource]}]}]})
        (flush)
        (assoc state :resource resource))))
                           
(defmethod iq [:session :set "urn:ietf:params:xml:ns:xmpp-session"]
  [content state]
  (do (xmpplog "opened session")
      (xml/emit-element {:tag :iq
                         :attrs {:id (-> content :attrs :id)
                                 :type "result"}
                         :content [{:tag :session
                                    :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-session"}}]})
      (flush)
      (assoc state :session true)))

(defmethod iq [:query :get "http://jabber.org/protocol/disco#items"]
  [content state]
  (do (xml/emit-element {:tag :iq
                        :attrs {:from (:domain conf)
                                :id (-> content :attrs :id)
                                :to (str (:username state) "@" (:domain conf) "/" (:resource state))
                                :type "result"}
                        :content [{:tag :query
                                   :attrs {:xmlns "http://jabber.org/protocol/disco#items"}}]})
      (flush)
      nil))

(defmethod iq [:query :get "http://jabber.org/protocol/disco#info"]
  [content state]
  (do (xml/emit-element {:tag :iq
                        :attrs {:from (:domain conf)
                                :id (-> content :attrs :id)
                                :to (str (:username state) "@" (:domain conf) "/" (:resource state))
                                :type "result"}
                        :content [{:tag :query
                                   :attrs {:xmlns "http://jabber.org/protocol/disco#info"}
                                   :content [{:tag :feature
                                              :attrs {:var "jabber:iq:roster"}}
                                             {:tag :feature
                                              :attrs {:var "vcard-temp"}}]}]})
      (flush)
      nil))

(defmethod iq [:query :get "jabber:iq:roster"]
  [content state]
  (let [friends (accounts/get-friends (:user-id state))]
    (do (xmpplog "requested roster [" (apply str (interpose ", " friends)) "]") 
        (xml/emit-element {:tag :iq
                           :attrs {:from (:domain conf)
                                   :id (-> content :attrs :id)
                                   :to (str (:username state) "@" (:domain conf) "/" (:resource state))
                                   :type "result"}
                           :content [{:tag :query
                                      :attrs {:xmlns "http://jabber.org/protocol/disco#info"}
                                      :content (for [friend friends]
                                                 {:tag :item
                                                  :attrs {:jid friend}})}]})
      (flush)
      nil)))

(defmethod iq [:ping :get "urn:xmpp:ping"]
  [content state]
  (do (xmpplog "ping!")
      (xml/emit-element {:tag :iq
                         :attrs {:id (-> content :attrs :id) 
                                 :from (:domain conf)
                                 :type "result"}})
      (flush)
      nil))
                                 

(defmethod iq :default
  [content state]
  (do (. log (warn (str "IP " (server/get-ip) " sent unknown message - MESSAGE " content)))
      nil))

