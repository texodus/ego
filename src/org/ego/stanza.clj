(ns org.ego.stanza
  (:gen-class)
  (:require [clojure.contrib.lazy-xml :as xml]
            [org.ego.common :as common]
            [org.ego.server :as server])
  (:use [org.ego.common :only [properties log]]))

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

(defstruct element :tag :attrs :content)
(defstruct stanza :stack :current :state :sb :count)

(defmulti parse (fn [stanza e] (:tag e)))

(defmethod parse :start-element
  [stanza el]
  (let [e (struct element 
                  (keyword (str (:qname el)))
                  (into {} (map #(vector (keyword (first %)) (second %)) (:attrs el))))]
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
    0 (server/close-channel)
    1 (dosync (push-chars stanza)
              (let [result [((:content (push-content (peek (:stack @stanza)) (:current @stanza))) 0)]]
                (alter-nil stanza assoc
                           :stack nil
                           :current (struct element)
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

(def #^{:private true
        :doc "Collections of open XML element buffers"}
     stanzas (ref {}))

(defmulti process 
  "Takes msg strings and returns a vector of generated element structs"
  (fn [event & _]  event))

(defmethod process :connect
  [_ ip]
  (dosync (alter-nil stanzas assoc ip (ref (struct stanza nil (struct element) :between nil 0)))))
  
(defmethod process :disconnect
  [_ ip]
  (dosync (alter-nil stanzas dissoc ip)))

(defmethod process :upstream
  [_ ip msg]
  (let [stanza (@stanzas ip)]
    (parse stanza msg)))

(defmethod process :downstream
  [_ ip msg]
  (emit msg))

(defmethod process :default [_ ip msg] msg)


