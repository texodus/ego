(ns org.ego.core.common
  (:gen-class)
  (:import [java.io FileReader BufferedReader InputStreamReader FileInputStream]
           [java.security SecureRandom]
	   [java.util Properties]))
 
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


