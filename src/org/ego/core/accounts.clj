(ns org.ego.core.accounts
  (:gen-class)
  (:import [java.net ServerSocket Socket SocketException InetAddress InetSocketAddress URL]
           [java.io InputStreamReader OutputStreamWriter PushbackReader ByteArrayInputStream Reader Writer OutputStream FileInputStream]
           [org.apache.log4j Logger])
  (:require [org.ego.core.common :as common]
            [clojure.contrib.sql :as sql])
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

(def accounts-online (ref {}))
(defstruct account-record :messages :listening)

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

;(defn- check-user-password
;  "Get a user's password hash"
;  [email password]
;  (sql/with-connection db
;    (sql/with-query-results rs 
;      [(str "SELECT id FROM accounts " 
;            "WHERE email = '" email "' "
;            "AND password = md5('" password "')")]
;      (if (= 1 (count rs)) (:id (first rs)) mil))))

(defn- set-user-last-login
  "Set a user's status in the DB"
  [email]
  (sql/with-connection db
    (sql/do-commands 
      (str "UPDATE accounts SET last_login_timestamp = (timestamp 'now') WHERE email='" email "'"))))

;(defn get-friends
;  "Gets the list of friends from the database"
;  [id]
;  (sql/with-connection (config/get-props "database")
;    (sql/with-query-results rs 
;      [(str-join \ ["SELECT accounts.email FROM accounts"
;                    "WHERE accounts.id IN"
;                    "(SELECT first_id FROM friends WHERE second_id = " id ")"
;                    "OR accounts.id IN"
;                    "(SELECT second_id FROM friends WHERE first_id = " id ")"])]
;       (doall (map :email rs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Internal

(defn- generate-messages
  "Generate a list of messages for an intended recip"
  [source target msg]
  (let [account (@accounts-online target)]
    (assoc account 
      :messages (assoc (account :messages) 
                  source (let [msg-q ((account :messages) source)]
                           (if (nil? msg-q)
                             (vector msg)
                             (let [new-q (conj msg-q msg)]
                               (if (> (count msg-q) (conf :notify-threshold))
                                 (apply vector (rest new-q))
                                 new-q))))))))

(defn- set-online
  "Sets the account to online"
  [email]
  (dosync
    (alter accounts-online 
	   assoc email (struct account-record {} {}))))

(defn- set-offline
  "Sets the account to offline"
  [email]
  (dosync
    (alter accounts-online 
	   dissoc email)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public

(defn logout
  "Logout the user"
  [email]
  (do
    (. log (debug (str "User " email " logged out")))
    (set-offline email)))

(defn get-avatar
  "Get avatar image for a user"
  [email]
  (str email ".png"))

(defn set-avatar
  "Writes a user avatar image to the filestore"
  [id avatar]
  (do (. log (debug (str-join \ ["Received file" (:filename avatar) "from user" id])))
      (common/copy-file (:tempfile avatar) (File. (str (:avatarroot dir) "/" id ".png")))))

(defn set-listening
  "Sets the account's listening status"
  [email song]
  (dosync
   (alter accounts-online assoc 
          email (assoc (@accounts-online email) 
                  :listening song))))

(defn online?
  "Checks to see if the account is online"
  [email]
  (not (nil? (@accounts-online email))))

(defn get-online-friends
  "Gets the list of online friends"
  [email]
  (reduce #(assoc %1 %2 {:messages (((@accounts-online email) :messages) %2)
                         :listening ((@accounts-online %2) :listening)})
          {}
          (filter online? (get-friends (get-id email)))))

(defn send-message
  "Sends a user a message"
  [sender-id recip-id msg]
  (do
    (. log (debug (str-join \  ["Message from" sender "to" recip ":" msg])))
    (dosync
     (alter accounts-online 
            assoc
            recip (generate-messages sender recip msg)
            sender (generate-messages recip sender 
                                      (html [:span {:style "color: #c03c13;"} msg]))))))