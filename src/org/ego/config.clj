(ns org.ego.common
  (:gen-class)
  (:import [java.io FileReader BufferedReader InputStreamReader FileInputStream]
	   [java.util Properties])
  (:use [clojure.contrib.str-utils :only (str-join)])
  (:require [clojure.contrib.sql :as sql]))

(def *application-context* :production)

(defn- read-properties [f]
   (into {}
    (let [props (Properties.)]
      (.load props (. ClassLoader (getSystemResourceAsStream f)))
      props)))

(defn get-properties
  "Generate a map of the elements from an application-context.namespace prefix from the properties file"
  [n]
  (apply hash-map
	 (apply concat
		(filter identity
			(let [ns (str (apply str (drop 1 (str *application-context*))) \. n)]
			  (for [key-val (read-properties "ego.properties")]
			    (if (. (first key-val) (startsWith ns))
			      (vector (keyword (. (first key-val) (replace (str ns \.) (str))))
				      (try (. Integer (parseInt (second key-val)))
					   (catch NumberFormatException e (second key-val)))))))))))