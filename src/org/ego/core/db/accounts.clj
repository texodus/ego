(ns org.ego.core.db.accounts
  (:gen-class)
  (:import [java.net ServerSocket Socket SocketException InetAddress InetSocketAddress URL]
           [java.io InputStreamReader OutputStreamWriter PushbackReader ByteArrayInputStream Reader Writer OutputStream FileInputStream]
           [org.apache.log4j Logger])
  (:require [org.ego.core.common :as common]
            [clojure.contrib.sql :as sql]
            [redis])
  (:use [org.ego.core.common :only [properties]]
        [clojure.contrib.logging :only [log]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(def db
     {:classname (properties :database:classname)
      :subprotocol (properties :database:subprotocol)
      :subname (properties :database:subname)
      :user (properties :database:user)
      :password (properties :database:password)})

(def db-redis {:host "127.0.0.1" :port 6379 :db 0})

(def accounts-online (ref {}))
(defstruct account-record :messages :listening)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Redis

(defn- get-user-id
  "Get a user's userid"
  [username]
  (Integer/parseInt (redis/get (str "username:" username ":userid"))))

(defn- get-user-password
  "Get a user's password"
  [userid]
  (redis/get (str "userid:" userid ":password")))

(defn login
  "Check the login credentials, set online in DB and return the user_id"
  [username password]
  (redis/with-server db-redis
    (let [id (get-user-id username)
          password-hash (get-user-password id)]
      (if (= password-hash (common/sha1 password))
        id
        nil))))

(defn get-friends
  "Returns a list of jids"
  [userid]
  (redis/with-server db-redis
    (redis/lrange (str "userid:" userid ":friendids") 0 -1)))

(defn gen-data
  []
  (redis/with-server db-redis
    (do (redis/set "username:andrew:userid" 1)
        (redis/set "userid:1:password" (common/sha1 "password"))
        (redis/lpush "userid:1:friendids" "wilfred@localhost")
        (redis/lpush "userid:1:friendids" "dave@localhost"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; SQL

;; (defn login
;;   "Check the login credentials, set online in DB and return the user_id"
;;   [username password]
;;   (sql/with-connection db
;;     (let [user-id (sql/with-query-results rs 
;;                     [(str "SELECT id FROM accounts " 
;;                           "WHERE username = ? "
;;                           "AND password = md5(?)") 
;;                           username
;;                           password]
;;                     (if (= 1 (count rs)) (:id (first rs)) nil))]
;;       (if (identity user-id)
;;         (do (sql/do-commands 
;;               (str "UPDATE accounts SET last_login_timestamp = (timestamp 'now') WHERE id = " user-id))
;;             user-id)
;;         nil))))

;; (defn get-friends
;;   "Returns a list of jids"
;;   [user-id]
;;   (let [query (str "SELECT jid FROM friends "
;;                    "WHERE account_id = " user-id)]
;;     (sql/with-connection db
;;       (sql/with-query-results rs
;;         [query]
;;         (doall (map :jid rs))))))

