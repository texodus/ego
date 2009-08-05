(ns org.ego.xmpp.stream
  (:gen-class)
  (:import [org.apache.commons.codec.binary Base64])
  (:require [org.ego.server :as server]
            [org.ego.db.accounts :as accounts]
            [org.ego.xmpp.iq :as iq]
            [org.ego.xmpp.message :as message])
  (:use [org.ego.common :only [properties gen-id]]
        [org.ego.server :only [log]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Process 

(defmulti parse (fn [content _] (:tag content)))

(defmethod parse :iq [content state] (iq/process content state))

(defmethod parse :message [content state] (message/process content state))

(defmethod parse :stream:stream
  [content state]
  (let [id (gen-id)]
    (log :info (str (:ip @state) " opened stream " id))
    ; Just assume the opening stream request was correct and open our own
    (dosync (alter state assoc :open true :id id))
    [(str "<stream:stream from='" (:server:domain properties)  "' id='" id
          "' xmlns:stream='http://etherx.jabber.org/streams' version='1.0' xmlns='jabber:client'>")
     {:tag :stream:features
      :content (filter identity
                       ; Only offer SSL if connection is in plaintext
                       [(if (not (state :ssl))
                          {:tag :starttls
                           :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-tls"}
                           :content [{:tag :required}]})
                        ; Only offer authentication if the user is not authenticated
                        (if (nil? (state :username))
                          {:tag :mechanisms
                           :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-sasl"}
                           :content [{:tag :mechanism
                                      :content ["PLAIN"]}
                                      ; TODO implement digest/sasl auth  
                                      ; {:tag :mechanism 
                                      ;  :content ["DIGEST-MD5"]}
                                     {:tag :required}]}
                          ; Otherwise offer bind and session
                          {:tag :bind
                           :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-bind"}
                           :content [{:tag :required}]})
                        (if (not (nil? (state :username)))
                          {:tag :session
                           :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-session"}
                           :content [{:tag :optional}]})])}]))

(defmethod parse :starttls
  [content state]
  (do (log :info (str (:ip @state) " switched to TLS"))
      (dosync (alter state assoc :ssl true))
      (try (server/start-tls)
           (catch Exception e (do (log :error "SSL failed" e)
                                  (server/close-channel))))
      [{:tag :proceed
        :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-tls"}}]))

(defmethod parse :auth
  [content state]
  (let [mechanism (-> content :attrs :mechanism)]
    (condp = (if (nil? mechanism) "PLAIN" mechanism)
      "PLAIN" (let [chars (. (Base64.) (decode (.getBytes (first (:content content)))))
                    username (apply str (map char (take-while pos? (drop 1 chars))))
                    password (apply str (map char (drop 1 (drop-while pos? (drop 1 chars)))))
                    user-id (accounts/login username password)]
                (if (nil? user-id)
                  (do (log :info (str (:ip @state) " failed to login as username " username))
                      [{:tag :failure
                        :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-sasl"}
                        :content [{:tag :temporary-auth-failure}]}])
                  (do (log :info (str (:ip @state) " logged in successfully as username " username))
                      (dosync (alter state assoc :username username :user-id user-id))
                      [{:tag :success
                        :attrs {:xmlns "urn:ietf:params:xml:ns:xmpp-sasl"}}]))))))

(defmethod parse :default
  [content state]
  (log :warn (str (:ip @state) " sent unknown " content)))
