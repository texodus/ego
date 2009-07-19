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
            [org.ego.db.accounts :as accounts]
            [org.ego.xmpp.iq :as iq]))
 

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Process 

(defmulti process (fn [content _] (:tag content)))

(defmethod process :iq [content state] (iq/process content state))

(defmethod process :stream:stream
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

(defmethod process :starttls
  [content state]
  (do (xmpplog "switched to TLS")
      (xml/emit-element {:tag :proceed
                         :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-tls"}})
      (try (server/start-tls)
           (catch Exception e (do (. log (error "SSL failed" e))
                                  (close-channel))))
      (flush)
      (assoc state :ssl true)))

(defmethod process :auth
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

(defmethod process :default
  [content state]
  (do (. log (warn (str "IP " (server/get-ip) " sent unknown " content)))
      nil))
