(ns org.ego.xmpp
  (:gen-class)
  (:import [java.net ServerSocket Socket SocketException InetAddress InetSocketAddress URL]
           [java.io InputStreamReader OutputStreamWriter PushbackReader ByteArrayInputStream Reader Writer OutputStream FileInputStream]
           [javax.xml.parsers SAXParserFactory]
           [org.xml.sax.helpers DefaultHandler]
           [org.xml.sax InputSource]
           [org.apache.log4j Logger]
           [clojure.lang LineNumberingPushbackReader]
           [sun.misc BASE64Decoder])
  (:require [clojure.contrib.lazy-xml :as xml]
            [org.ego.config :as config]
            [org.ego.server :as server]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(def #^{:private true} log (. Logger (getLogger (str *ns*))))
(def #^{:private true} conf (config/get-properties "server"))

(defstruct stream-state-record :open :ssl :stanza :id)
(defstruct session-state-record :username :ip)

(def *stream-state* nil)
(def *session-state* nil)
(def id-counter (ref 1))

; TODO make an unpredictable (nonsequential) id generator
(defn gen-id
  []
  (do (dosync (alter id-counter inc))
      @id-counter))

(defn- base64-decode 
  [string]
  (.trim (.decode (BASE64Decoder.) 
                  (.getBytes string))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; XML

(defn map-attributes
  [attrs]
  (loop [index 0, attr-map {}]
    (if (= index (. attrs getLength))
      attr-map
      (recur (inc index)
             (assoc attr-map
               (. attrs (getQName index))
               (. attrs (getValue index)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; XMPP 

;;;;
;;;; Open
;;;;

(defmulti process-open (fn [qname attrs] qname))

(defmethod process-open "stream:stream"
  [_ attrs]
  (do (dosync (alter *stream-state* assoc :open true))
      (. log (info (str "IP " (:ip @*session-state*) " opened stream")))
      ; Just assume the opening stream request was correct and open our own
      (print (str "<stream:stream from='" (:domain conf)  "' id='" (@*stream-state* :id)
                  "' xmlns:stream='http://etherx.jabber.org/streams' version='1.0' xmlns='jabber:client'>"))
      (xml/emit-element {:tag :stream:features
                         :content (filter identity
                                          [(if (not (@*stream-state* :ssl)) 
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
      (flush)))

(defmethod process-open :default
  [qname attrs]
  (do (. log (debug (str "IP " (:ip @*session-state*) " sent " qname)))
      nil))

;;;;
;;;; Close
;;;;

(defmulti process-close (fn [qname] qname))

(defmethod process-close "stream:stream"
  [_]
  (do (print "</stream:stream>")
      (. log (info (str "IP " (:ip @*session-state*) " closed stream")))
      (dosync (alter *stream-state* assoc :open false))))

(defmethod process-close "starttls"
  [_]
  (do (xml/emit-element {:tag :proceed
                         :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-tls"}})
      (server/start-tls)
      (dosync (alter *stream-state* assoc :ssl true))))


(defmethod process-close :default [& _] nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Parser

(def #^{:private true} xmpp-handler
     (proxy [DefaultHandler] []
       (startElement [uri local qname atts]
                     (process-open qname (map-attributes atts)))
       (endElement [uri local qname]
                   (process-close qname))
       (characters [chars start length])))

(defn get-xmpp-parser
  "Create an XMPP parser for *in* stream"
  []
  (binding [*stream-state* (ref (struct stream-state-record false false nil (gen-id)))
            *session-state* (ref (struct session-state-record nil (server/get-ip)))]
    (.. SAXParserFactory newInstance newSAXParser
        (parse (InputSource. *in*)
               xmpp-handler))))
