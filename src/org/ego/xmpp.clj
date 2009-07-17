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
            [org.ego.accounts :as accounts]))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(def #^{:private true} log (. Logger (getLogger (str *ns*))))
(def #^{:private true} conf (config/get-properties "server"))

(defn xmpplog
  [& string]
  (. log (info (str "IP " (server/get-ip) " " (apply str string)))))

(defstruct stream-state-record :open :ssl :id :authenticated)

(def new-stream-state (struct stream-state-record false false nil false))

(def id-counter (ref 1))

; TODO make an unpredictable (nonsequential) id generator
(defn gen-id
  []
  (do (dosync (alter id-counter inc))
      @id-counter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; XMPP 

(defmulti process-xmpp (fn [content _] (:tag content)))

(defmethod process-xmpp :stream:stream
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
                                         (if (not (state :authenticated))
                                           {:tag :mechanisms
                                            :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-sasl"}
                                            :content [{:tag :mechanism
                                                       :content ["PLAIN"]}
                                                    ; TODO implement digest/sasl auth  
                                                    ; {:tag :mechanism 
                                                    ;  :content ["DIGEST-MD5"]}
                                                      {:tag :required}]})])})
    (flush)
    (assoc state :open true :id id)))

(defmethod process-xmpp :starttls
  [content state]
  (do (xmpplog "switched to TLS")
      (xml/emit-element {:tag :proceed
                         :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-tls"}})
      (server/start-tls)
      (flush)
      (assoc state :ssl true)))

(defmethod process-xmpp :auth
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
                    (assoc state :authenticated true))))
    state))

(defmethod process-xmpp :default
  [content state]
  (do (. log (warn (str "IP " (server/get-ip) " sent unknown message " content)))
      state))

