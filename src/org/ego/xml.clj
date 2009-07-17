(ns org.ego.xml
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; XML

(defn- map-attributes
  [attrs]
  (loop [index 0, attr-map {}]
    (if (= index (. attrs getLength))
      attr-map
      (recur (inc index)
             (assoc attr-map
               (keyword (. attrs (getQName index)))
               (. attrs (getValue index)))))))



(defstruct element :tag :attrs :content)

(def *stack*)
(def *current*)
(def *state*)
(def *sb*)
(def *count*)
(def *stream-state*)

(defn emit-element
  [content]
  (xml/emit-element content)) 

(defn parse
  "Create an XMPP parser for *in* stream"
  [stanza-fun initial-state]
  (binding [*stack* nil
            *current* (struct element)
            *state* :between
            *sb* nil
            *count* 0
            *stream-state* initial-state]
    (.. SAXParserFactory newInstance newSAXParser
        (parse (InputSource. *in*)
               (let [push-content (fn [e c]
                                    (assoc e :content (conj (or (:content e) []) c)))
                     push-chars (fn []
                                  (when (and (= *state* :chars)
                                             (some (complement #(. Character (isWhitespace %))) (str *sb*)))
                                    (set! *current* (push-content *current* (str *sb*)))))]
                 (proxy [DefaultHandler] []
                   (startElement [uri local-name qname #^Attributes atts]
                                 (let [e (struct element 
                                                 (keyword qname)
                                                 (map-attributes atts))]
                                   (if (= (:tag e) :stream:stream)
                                     (set! *stream-state* (stanza-fun e *stream-state*))
                                     (do (push-chars)
                                         (set! *count* (inc *count*))
                                         (set! *stack* (conj *stack* *current*))
                                         (set! *current* e)
                                         (set! *state* :element)
                                         nil))))
                   (endElement [uri local-name q-name]
                               (condp = *count*
                                 0 (server/close-channel)
                                 1 (do (push-chars)
                                       (let [new-state (stanza-fun ((:content (push-content (peek *stack*) *current*)) 0) 
                                                         *stream-state*)]
                                         (if (not (nil? new-state))
                                           (set! *stream-state* new-state)))
                                       (set! *stack* nil)
                                       (set! *current* (struct element))
                                       (set! *state* :between)
                                       (set! *sb* nil)
                                       (set! *count* 0))
                                 (do (push-chars)
                                     (set! *count* (dec *count*))
                                     (set! *current* (push-content (peek *stack*) *current*))
                                     (set! *stack* (pop *stack*))
                                     (set! *state* :between)
                                     nil)))
                   (characters [ch start length]
                               (when-not (= *state* :chars)
                                 (set! *sb* (new StringBuilder)))
                               (let [#^StringBuilder sb *sb*]
                                 (. sb (append ch start length))
                                 (set! *state* :chars))
                               nil)))))))