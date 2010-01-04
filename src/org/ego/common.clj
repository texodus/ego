(ns org.ego.common
  (:gen-class)
  (:import [java.io FileReader BufferedReader BufferedInputStream BufferedOutputStream FileOutputStream InputStreamReader FileInputStream]
           [java.security SecureRandom MessageDigest]
	   [java.util Properties])
  (:require [clojure.contrib.logging :as logging]))
 
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

(defn log
  [lvl msg]
  (logging/log lvl msg))

(def #^{:private true} seed (new SecureRandom))

(defn gen-id [] (. (BigInteger. 50 seed)
                   (toString 32)))

(defn copy-file
  [source dest]
  (with-open [r (BufferedInputStream. (FileInputStream. source))
	      w (BufferedOutputStream. (FileOutputStream. dest))]
    (loop [c (.read r)]
      (if (neg? c)
	nil
	(do (. w write c)
	    (recur (.read r)))))))

(defn- translate-halfbyte
  [halfbyte]
  (if (and (<= 0 halfbyte) (<= halfbyte 9))
    (char (+ (int \0) halfbyte))
    (char (+ (int \a) (- halfbyte 10)))))

(defn sha1
  [text]
  (let [md (MessageDigest/getInstance "SHA-1")
        hex (do (.update md (.getBytes text "iso-8859-1") 0 (count text))
                (.digest md))]
    (loop [[x & xs] hex
           result   ""]
      (if (nil? x)
        result
        (recur xs (str result (let [halfbyte (bit-and (bit-shift-right x 4) 0x0F)]
                                (apply str (map translate-halfbyte [halfbyte (bit-and x 0x0F)])))))))))

(defmacro alter-nil
  "Works like alter but returns nil"
  [& xs]
  `(do (alter ~@xs) nil))

(defn parse-jid
  [string]
  (if (empty? string)
    (vector nil nil nil)
    (if (. string (contains "@"))
    (let [parts (. string (split "@"))]
      (if (. (second parts) (contains "/"))
        (cons (first parts) (. (second parts) (split "/")))
        (vector (first parts) (second parts) nil)))
    (vector nil string nil))))




