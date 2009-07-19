(ns org.ego.xmpp
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

(defn- close-channel
  []
  (do (print "</stream:stream>")
      (flush)
      (server/close-channel)))

(defstruct stream-state-record 
  :open :ssl :id 
  :resource :session :username
  :user-id)

(def new-stream-state (struct stream-state-record 
                              false false nil 
                              nil true nil
                              nil))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (do (xmpplog "requested roster") 
      (xml/emit-element {:tag :iq
                         :attrs {:from (:domain conf)
                                 :id (-> content :attrs :id)
                                 :to (str (:username state) "@" (:domain conf) "/" (:resource state))
                                 :type "result"}
                         :content [{:tag :query
                                    :attrs {:xmlns "http://jabber.org/protocol/disco#info"}
                                    :content (for [friend (accounts/get-friends (:user-id state))]
                                               {:tag :item
                                                :attrs {:jid friend}})}]})
      (flush)
      nil))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; XMPP 

(defmulti xmpp (fn [content _] (:tag content)))

(defmethod xmpp :iq [content state] (iq content state))

(defmethod xmpp :stream:stream
  [content state]
  (let [id (gen-id)]
    (xmpplog "opened stream " id)
    ; Just assume the opening stream request was correct and open our own
    (print (str "<stream:stream from='" (:domain conf)  "' id='" id
                "' xmlns:stream='http://etherx.jabber.org/streams' version='1.0' xmlns='jabber:client'>"))
    (xml/emit-element {:tag :stream:features
                       :content (filter identity
                                        ; Only offer SSL if connection is in plaintext
                                        [(if (not (state :ssl)) 
                                           {:tag :starttls
                                            :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-tls"}
                                            :content [{:tag :required}]})
                                         ; Only offer authentication if the user is not authenticated
                                         (if (nil? (state :username))
                                           {:tag :mechanisms
                                             :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-sasl"}
                                             :content [{:tag :mechanism
                                                        :content ["PLAIN"]}
                                                      ; TODO implement digest/sasl auth  
                                                      ;{:tag :mechanism 
                                                      ; :content ["DIGEST-MD5"]}
                                                       {:tag :required}]}
                                          ; Otherwise offer bind and session
                                            {:tag :bind
                                             :attrs {:xmlns "urn:ietf:params:sml:ns:xmpp-bind"}
                                             :content [{:tag :required}]})
                                         (if (not (nil? (state :username)))
                                           {:tag :session
                                            :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-session"}
                                            :content [{:tag :optional}]})])})
    (flush)
    (assoc state :open true :id id)))

(defmethod xmpp :starttls
  [content state]
  (do (xmpplog "switched to TLS")
      (xml/emit-element {:tag :proceed
                         :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-tls"}})
      (try (server/start-tls)
           (catch Exception e (do (. log (error "SSL failed" e))
                                  (close-channel))))
      (flush)
      (assoc state :ssl true)))

(defmethod xmpp :auth
  [content state]
  (condp = (-> content :attrs :mechanism)
    "PLAIN" (let [chars (. (Base64.) (decode (.getBytes (first (:content content)))))
                  username (apply str (map char (take-while pos? (drop 1 chars))))
                  password (apply str (map char (drop 1 (drop-while pos? (drop 1 chars)))))
                  user-id (accounts/login username password)]
              (if (nil? user-id)
                (do (xmpplog "failed to login as username " username)
                    (xml/emit-element {:tag :failure
                                       :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-sasl"}
                                       :content [{:tag :temporary-auth-failure}]})
                    (flush)
                    state)
                (do (xmpplog (str "logged in successfully as username " username))
                    (xml/emit-element {:tag :success
                                       :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-sasl"}})
                    (flush)
                    (assoc state :username username :user-id user-id))))
    nil))

(defmethod xmpp :default
  [content state]
  (do (. log (warn (str "IP " (server/get-ip) " sent unknown message " content)))
      nil))
