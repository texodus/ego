(ns com.texodus.centrifuge.application
  (:gen-class)
  (:import [org.apache.log4j Logger])
  (:use [compojure])
  (:require [clojure.contrib.sql :as sql]
            [clojure.contrib.json.write :as json]
	    [org.ego.core.accounts :as accounts]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; JSON

(defn- set-avatar
  [{id :account-id} {avatar :avatar_file}]
  (do (accounts/set-avatar id avatar)
      (json/json-str {:status "success"})))

(defn- get-avatar
  [{id :account-id}]
  (serve-file (:avatarroot conf) 
	      (accounts/get-avatar id)))

(defn- send-message
  [{sender :account-id} {recip :contact_id msg :contact_message}]
  (do (accounts/send-message sender recip msg)
      (json/json-str {:status "success"})))

(defn- login
  "Validate a user logging in"
  [session {email :name, password :password}]
  (let [id (accounts/login email password)]
    (if (nil? id)
      (json/json-str {:status "failure"})
      {:session (assoc session :account-id id)
       :body (json/json-str {:status "success"})})))


(defn- check-status
  "Check the user's status"
  [session]
  (if (nil? (session :email))
      (json/json-str {:status "failure"})
      (json/json-str {:status "success"})))

(defn- logout
  "Logout the user"
  [session params]
  (do (accounts/logout (session :email))
      {:session (dissoc session :email)
       :body (json/json-str {:status "success"})}))

(defn- get-online-friends 
  [{email :email}]
  (json/json-str (accounts/get-online-friends email)))

(defn- set-song
  [{email :email} {song :song_file art :song_art_file title :song_title band :song_band}]
  (do (songs/set-song email song art title band)
      (json/json-str {:status "success"})))

(defn- get-song
  [{email :email} {id :id}]  
  (let [f (songs/get-song email id)]
    (json/json-str {:headers {"Content-Length" (str (. f length))}
                    :body f})))

(defn- get-song-list
  []
  (json/json-str (songs/get-song-list)))

(defn- get-song-image
  [params]
  (serve-file (:artroot conf) (str (:id params) ".jpg")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Server

(defroutes ego-servlet
  ; Static Content
  (GET "/" (redirect-to "/index.html"))
  (GET "/*" (or (serve-file (:webroot conf) (:* params)) :next))
  ; Posts
  (POST "/avatar" (set-avatar session params))
  (POST "/songupload" (set-song session params))
  (POST "/message" (send-message session params))
  (POST "/login" (login session params))
  (ANY "/logout" (logout session params))
  ; Models
  (GET "/avatar" (get-avatar params))
  (GET "/song" (get-song session params))
  (GET "/songlist" (get-song-list))
  (GET "/songimage" (get-song-image params))
  (GET "/friends" (get-online-friends session))
  (GET "/login" (check-status session))
  ; Error
  (ANY "*" (page-not-found)))


