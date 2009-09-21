(ns org.ego.db.accounts
  (:gen-class)
  (:import [java.net ServerSocket Socket SocketException InetAddress InetSocketAddress URL]
           [java.io InputStreamReader OutputStreamWriter PushbackReader ByteArrayInputStream Reader Writer OutputStream FileInputStream]
           [org.apache.log4j Logger])
  (:require [org.ego.common :as common]
            [clojure.contrib.sql :as sql])
  (:use [org.ego.common :only [properties]]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; SQL

(defn login
  "Check the login credentials, set online in DB and return the user_id"
  [username password]
  (sql/with-connection db
    (let [user-id (sql/with-query-results rs 
                    [(str "SELECT id FROM accounts " 
                          "WHERE username = '" username "' "
                          "AND password = md5('" password "')")]
                    (if (= 1 (count rs)) (:id (first rs)) nil))]
      (if (identity user-id)
        (do (sql/do-commands 
              (str "UPDATE accounts SET last_login_timestamp = (timestamp 'now') WHERE id = " user-id))
            user-id)
        nil))))

(defn get-friends
  "Returns a list of jids"
  [user-id]
  (let [query (str "SELECT jid FROM friends "
            "WHERE account_id = " user-id)]
    (sql/with-connection db
      (sql/with-query-results rs
        [query]
        (doall (map :jid rs))))))