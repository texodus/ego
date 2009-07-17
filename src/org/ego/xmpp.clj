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
            [org.ego.server :as server]))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(def #^{:private true} log (. Logger (getLogger (str *ns*))))
(def #^{:private true} conf (config/get-properties "server"))

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
    (. log (info (str "IP " (server/get-ip) " opened stream")))
    ; Just assume the opening stream request was correct and open our own
    (print (str "<stream:stream from='" (:domain conf)  "' id='" id
                "' xmlns:stream='http://etherx.jabber.org/streams' version='1.0' xmlns='jabber:client'>"))
    (xml/emit-element {:tag :stream:features
                       :content (filter identity
                                        [(if (not (state :ssl)) 
                                           {:tag :starttls
                                            :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-tls"}
                                            :content [{:tag :required}]})
                                         {:tag :mechanisms
                                          :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-sasl"}
                                          :content [{:tag :mechanism
                                                     :content ["PLAIN"]}
                                        ; {:tag :mechanism
                                        ;  :content ["DIGEST-MD5"]}
                                                    {:tag :required}]}])})
    (flush)
    (assoc state :open true :id id)))

(defmethod process-xmpp :starttls
  [content state]
  (do (. log (info (str "IP " (server/get-ip) " switched to TLS")))
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
                  password (apply str (map char (drop 1 (drop-while pos? (drop 1 chars)))))]
              (. log (info (str "Username: " username " Password: " password))))))

(defmethod process-xmpp :default
  [content state]
  (do (. log (warn (str "IP " (server/get-ip) " sent unknown message " content)))
      state))

