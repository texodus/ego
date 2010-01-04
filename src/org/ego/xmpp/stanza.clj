(ns org.ego.xmpp.stanza
  (:gen-class)
  (:require ;[clojure.contrib.lazy-xml :as xml]
            [org.ego.common :as common]
            [org.ego.xmpp :as server])
  (:use [org.ego.common :only [properties]]
        [clojure.contrib.logging :only [log]]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(defmacro alter-nil
  "Works like alter but returns nil"
  [& xs]
  `(do (alter ~@xs) nil))

(defn- push-content 
  [e c]
  (assoc e :content (conj (or (:content e) []) c)))

(defn- push-chars 
  [stanza]
  (when (and (= (:state @stanza) :chars)
             (some (complement #(. Character (isWhitespace %))) (str (:sb @stanza))))
    (alter-nil stanza assoc :current (push-content (:current @stanza) (str (:sb @stanza))))))

(defn emit [e]
  (if (instance? String e)
    e
    (str "<" 
         (name (:tag e))
         (when (:attrs e)
           (apply str (for [attr (:attrs e)]
                        (str " " (name (key attr)) "='" (val attr)"'"))))
         (if (:content e)
           (str ">"
                (apply str (for [c (:content e)]
                             (emit c)))
                (str "</" (name (:tag e)) ">"))
           "/>"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; XML parser

(deftype Element [tag attrs content] clojure.lang.IPersistentMap)
(deftype Stanz [stack current state sb count ip] clojure.lang.IPersistentMap)

(defmulti parse (fn [stanza e] (:tag e)))

(defmethod parse :start-element
  [stanza el]
  (let [e (Element (keyword (str (:qname el)))
                   (into {} (map #(vector (keyword (first %)) (second %)) (:attrs el)))
                   [])]
    (if (= (:tag e) :stream:stream)
      [e] ; this is a stream, pass and don't acc
      (dosync (push-chars stanza)
          (alter-nil stanza assoc 
                     :count (inc (:count @stanza))
                     :stack (conj (:stack @stanza) (:current @stanza))
                     :current e
                     :state :element)))))

(defmethod parse :end-element
  [stanza el]
  (condp = (:count @stanza)
    0 (server/close-channel (:ip @stanza))
    1 (dosync (push-chars stanza)
              (let [result [((:content (push-content (peek (:stack @stanza)) (:current @stanza))) 0)]]
                (alter-nil stanza assoc
                           :stack nil
                           :current (Element :none {} [])
                           :state :between
                           :sb nil
                           :count 0)
                result))
    (dosync (push-chars stanza)
            (alter-nil stanza assoc
                       :count (dec (:count @stanza))
                       :current (push-content (peek (:stack @stanza)) (:current @stanza))
                       :stack (pop (:stack @stanza))
                       :state :between))))

(defmethod parse :characters
  [stanza el]
  (dosync (when-not (= (:state @stanza) :chars)
            (alter stanza assoc :sb (new StringBuilder)))
          (let [#^StringBuilder sb (:sb @stanza)]
            (. sb (append (:qname el)))
            (alter-nil stanza assoc :state :chars))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Channel Handler

(def #^{:private true} stanzas (ref {}))

(deftype Stanza []
  server/Server (connect [ip] (dosync (alter-nil stanzas assoc 
                                            ip (ref (Stanz nil (Element :none {} []) :between nil 0 ip)))))
                (disconnect [ip] (dosync (alter-nil stanzas dissoc ip)))
                (upstream [ip msg] (let [stanza (@stanzas ip)]
                                     (parse stanza msg)))
                (downstream [ip msg] (emit msg)))
