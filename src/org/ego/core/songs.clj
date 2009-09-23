(ns com.texodus.centrifuge.core.songs
  (:gen-class)
  (:import [java.io File BufferedInputStream BufferedOutputStream FileInputStream FileOutputStream]
           [java.net URL]
           [org.apache.log4j Logger])
  (:use [clojure.contrib.str-utils :only (str-join)]
        [compojure]
        [clojure.contrib.duck-streams]
        [clojure.xml])
  (:require [clojure.contrib.sql :as sql]
	    [com.texodus.centrifuge.config :as config]
            [com.texodus.centrifuge.common :as common]
            [com.texodus.centrifuge.core.accounts :as accounts]
            [clj-http-client.core :as http-client]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Locals

(def #^{:private true} log (. Logger (getLogger (str *ns*))))
(def #^{:private true} conf (config/get-props "dir"))
(def #^{:private true} db (config/get-props "database"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Database

(defn get-song-internal
  [id]
   (sql/with-connection db
     (sql/with-query-results rs 
       [(str-join \  ["SELECT songs.id, songs.title, bands.name"
                      "FROM songs"
                      "JOIN albums"
                      "ON songs.album_id = albums.id"
                      "JOIN bands"
                      "ON albums.band_id = bands.id"
                      "WHERE songs.id =" id])]
       (first rs))))

(defn create-song
  "Creates or updates the song record and returns it's id"
  [title band]
  (sql/with-connection db
   (do
     (let [band-id (sql/with-query-results rs 
                     [(str "SELECT * FROM bands "
			   "WHERE name LIKE '" band "'")] 
                     (if (empty? rs)
                       (do (sql/insert-values :bands [:name] [band])
                           (sql/with-query-results r 
			     ["SELECT currval('bands_primary_seq')"] 
			     (:currval (first r))))
                       (:id (first rs))))]
       (sql/insert-values :songs
                          [:title :band_id]
                          [(str title) band-id])
       (sql/with-query-results rs 
         ["SELECT currval('songs_primary_seq')"]
         (:currval (first rs)))))))

(defn- get-songs
  []
  (sql/with-connection db
    (sql/with-query-results rs 
      [(str-join \  ["SELECT songs.id, songs.title, bands.name" 
		     "FROM songs"
		     "JOIN bands"
		     "ON bands.id = songs.band_id"
		     "LIMIT 50"])]
      (doall (map (fn [x] (assoc x 
			    :title (apply str (take 15 (:title x)))
			    :name (apply str (take 15 (:name x)))))
		  rs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Utility

(defn- get-image-url
  "Google Image search the band's name and regex for the first image url"
  [band-name]
  (re-find #"(?<=<img src=).+?(?= .+?/img)" 
           (last (http-client/http-get (str "http://images.google.com/images?q=" 
                                            (. band-name (replaceAll " " "%20"))
                                            "%20sxsw")))))

(defn- download-image
  "Download an image of band-name to dest"
  [dest band-name]
  (with-open [w (FileOutputStream. dest)]
    (. w write (last (http-client/http-get-bytes (get-image-url band-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Importer

(defn import-sxsw
  [dir year]
  (do
    (. log (info (str "Starting SXSW sync job for directory " dir)))
    (for [file-name (map #(. % (replaceAll "_" " ")) (. (File. dir) list))]
      (let [band (first (. file-name (split "-")))
            song (first (. (apply str (rest (. file-name (split "-")))) (split ".mp3")))
            album (str "SXSW " year)
            id (create-song song band album)]
        (. log (debug  (str band " : " song)))
        (try (download-image (str (:artroot conf) "/" id ".jpg") band)
             (catch Exception e (common/copy-file (str (:webroot conf) "/img/brooklyn.jpg")
                                                      (str (:artroot conf) "/" id ".jpg"))))
        (common/copy-file (str dir "/" (. file-name (replace " " "_" )))
                              (str (:songroot conf) "/" id ".mp3"))
        (. Thread (sleep 5000))))
    (. log (info "Finished SXSW sync job"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public

(defn set-song
  "Receive a song bundle from the user and add to system"
  [email song art title band]
  (do (. log (debug (str-join \ ["Received file" (:filename song) "and" (:filename art) "from user" email])))
      (let [song-id (create-song title band)]
        (do (common/copy-file (:tempfile song) 
                              (File. (str (:songroot conf) "/" song-id ".mp3")))
            (common/copy-file (:tempfile art) 
                              (File. (str (:artroot conf) "/" song-id ".jpg")))))))

(defn get-song
  "Serve a song given a song id"
  [email id]
  (do
    (accounts/set-listening email (get-song-internal id))
    (File. (str (:songroot conf) "/" id ".mp3"))))

(defn get-song-list
  "Return a list of songs given some criteria"
  []
  (get-songs))


