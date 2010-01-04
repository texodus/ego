(ns org.ego.xmpp.xml
  (:gen-class)
  (:require [org.ego.xmpp.server :as server])
  (:use [org.ego.core.common :only [properties]]
        [clojure.contrib.logging :only [log]]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(defn- strip-lines
  "Remove newlines from a string"
  [text]
  (apply str (filter #(not (or (= (int %) 13) (= (int %) 10))) text)))

(defmacro alter-nil
  "Works like alter but returns nil"
  [& xs]
  `(do (alter ~@xs) nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Character Parser

(defstruct element :state :tag :qname :attrs)

(defmulti parse
  "Parse a single character in an xml stream"
  (fn [element & x] (:state @element)))

(defmethod parse nil                                                                ; don't know the current state
  [element c]
  (if (not (or (= c \newline) (= c \space) (= c \tab) (= c (first "\r"))))
    (if (= c \<)
      (alter-nil element assoc 
                 :state :pre-qname :tag :start-element 
                 :qname nil :attrs [])                                              ; this is an XML element
      (alter-nil element assoc 
                 :state :characters :tag :characters 
                 :qname c :attrs []))))                                             ; this is character data

(defmethod parse :pre-qname                                                         ; element has just started
  [element c]
  (condp = c 
    \/ (alter-nil element assoc :state :qname, :tag :end-element)                   ; this is an end tag
    \? (alter-nil element assoc :state :dtd)
    (alter-nil element assoc :state :qname, :qname c, :tag :start-element)))        ; type element is undetermined

(defmethod parse :dtd
  [element c]
  (if (= c \>) (alter-nil element assoc :state nil)))

(defmethod parse :characters                                                        ; parsing character data
  [element c]
  (if (= c \<)
    (let [return (alter element assoc :state :pre-qname)]                           ; character data is over
      (alter-nil element assoc :qname nil)
      return)
    (alter-nil element assoc :qname (str (:qname @element) c))))                    ; more character data
        
(defmethod parse :qname                                                             ; parsing qname
  [element c]
    (condp = c
      \> (alter element assoc :state nil)                                           ; this is the end of a start-element
      \  (alter-nil element assoc :state :between)                                  ; qname is finished, switching to attr-name
      \/ (alter element assoc :tag :start-element, :state :must-end)                ; qname is finished, switching to must-terminate
      (alter-nil element assoc :qname (str (:qname @element) c))))                  ; more qname characters
  
(defmethod parse :between                                                           ; between xml terms
  [element c]
  (condp = c
    \/ (alter element assoc :tag :start-element, :state :must-end)                  ; element is finished, switch to must-terminate
    \  nil                                                                          ; no-op
    \> (alter element assoc :state nil)                                             ; end of element
    (alter-nil element assoc                                                        ; first attr-name character
               :state :attr-name 
               :attrs (cons [c ""] (:attrs @element))))) 

 (defmethod parse :must-end                                                         ; parsed a / but not terminated
   [element c]
   (if (= c \>)
     (alter element assoc :tag :end-element :state nil)                             ; parsed a / but not terminated
     (alter element assoc :state nil :tag :malformed)))                             ; malformed

(defmethod parse :attr-name                                                         ; element is malformed
  [element c]
  (dosync (condp = c 
            \  nil                                                                  ; attr-name is over, no-op
            \= (alter-nil element assoc :state :pre-attr-value)                     ; attr-name is over, switch to pre-attr-value
            (alter-nil element assoc                                                ; more attr-name characters
                       :attrs (cons [(str (first (first (:attrs @element))) c) ""] 
                                    (drop 1 (:attrs @element)))))))

(defmethod parse :pre-attr-value                                                    ; finished parsing an attr-name but not started a value yet
  [element c]
  (dosync (condp = c
            \" (alter-nil element assoc :state :attr-value)                         ; start an attr-value
            \'  (alter-nil element assoc :state :attr-value)
            (alter element assoc :state nil :tag :malformed))))                     ; malformed

(defmethod parse :attr-value                                                        ; parsing attribute value
  [element c]
  (dosync (condp = c
            \" (alter-nil element assoc :state :between)
            \' (alter-nil element assoc :state :between)
            (alter-nil element assoc                                                ; more attr-value characters
                       :attrs (cons [(first (first (:attrs @element))) 
                                     (str (second (first (:attrs @element))) c)]
                                    (drop 1 (:attrs @element)))))))
    
(defmethod parse :default
  [element c]
  (dosync (alter element assoc :state nil :tag :malformed)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Channel Handler

(def #^{:private true} elements (ref {}))

(deftype XML []
  server/Server (connect [ip] (dosync (alter-nil elements assoc ip (ref (struct element nil nil nil [])))))
                (disconnect [ip] (dosync (alter-nil elements dissoc ip)))
                (upstream [ip msg] (let [element (@elements ip)]
                                     (dosync (loop [tokens msg return []]
                                               (if (not (empty? tokens))
                                                 (recur (rest tokens)
                                                        (conj return (parse element (first tokens))))
                                                 (filter identity return))))))
                (downstream [ip msg] msg))