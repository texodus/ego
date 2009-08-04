(ns org.ego.common
  (:gen-class)
  (:import [java.io FileReader BufferedReader InputStreamReader FileInputStream]
           [java.security SecureRandom]
	   [java.util Properties]))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Logging

(defstruct log-system
  :name     ; the name of the logging system used
  :get-log  ; fn [name] to obtain a log by string name
  :enabled? ; fn [log lvl] to check if a particular level is emabled
  :write)   ; fn [log lvl msg ex] to a log a message

(defmacro direct-log
  "Logs the message immediately if the specific logging level is enabled. Use 
  the log macro in prefernce to this."
  [level message throwable log-name system]
  `(let [log# ((~system :get-log) ~log-name)]
    (if ((~system :enabled?) log# ~level)
      ((~system :write) log# ~level ~message ~throwable))))

(defmacro send-log
  "Sends the message to a logging agent. Use the log macro in preference to 
  this."
  [level message throwable log-name system-agent]
 `(send-off ~system-agent 
            (fn [sys# lvl# msg# th# ln#]
               (let [log# ((sys# :get-log) ln#)]
                 (if ((sys# :enabled?) log# lvl#)
                   ((sys# :write) log# lvl# (force msg#) th#))
                 sys#))
            ~level (delay ~message) ~throwable ~log-name))

(defmacro log4j-logging
  "Creates a log-system struct using the log4j API, if present; otherwise nil."
  []
  (try
    (import (org.apache.log4j Logger Level))
    `(let [levels# {:debug Level/DEBUG
                    :info  Level/INFO
                    :warn  Level/WARN
                    :error Level/ERROR
                    :fatal Level/FATAL}]
      (letfn [(get-log# [name#] 
                (Logger/getLogger name#))
              (enabled?# [log# level#] 
                (.isEnabledFor log# (levels# level#)))
              (write# [log# level# msg# e#]
                (if-not e#
                  (.log log# (levels# level#) msg#)
                  (.log log# (levels# level#) msg# e#)))]
        (struct log-system "log4j-logging" get-log# enabled?# write#)))
    (catch Exception e nil)))

(def *log-system*
  (ref (log4j-logging)))
        
(def *log-system-agent* (agent *log-system*))

(def *allow-direct-logging* (ref false))

(defmacro log
  "Logs a message, either directly or via an agent."
  ([level message]
    `(log ~level ~message nil))
  ([level message throwable]
    `(do (if (and @*allow-direct-logging*
                  (not (clojure.lang.LockingTransaction/isRunning)))
           (direct-log ~level ~message ~throwable ~(list 'str '*ns*) @*log-system*)
           (send-log ~level ~message ~throwable ~(list 'str '*ns*) *log-system-agent*))
         nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Properties

(def *application-context* :production)

(defn- read-properties [f]
   (into {}
    (let [props (Properties.)]
      (.load props (. ClassLoader (getSystemResourceAsStream f)))
      props)))

(def properties
     (apply hash-map
            (apply concat
                   (filter identity
                           (let [ns (apply str (drop 1 (str *application-context*)))]
                             (for [key-val (read-properties "ego.properties")]
                               (if (. (first key-val) (startsWith ns))
                                 (vector (keyword (.. (first key-val) 
                                                      (replace (str ns \.) (str))
                                                      (replace \. \:)))
                                         (try (. Integer (parseInt (second key-val)))
                                              (catch NumberFormatException e (second key-val)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Misc.

(def #^{:private true} seed (new SecureRandom))

(defn gen-id [] (. (BigInteger. 50 seed)
                   (toString 32)))


