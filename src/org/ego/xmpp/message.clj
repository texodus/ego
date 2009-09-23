(ns org.ego.xmpp.message
  (:gen-class)
  (:require [org.ego.core.common :as common]
            [org.ego.xmpp.server :as server]
            [org.ego.core.accounts :as accounts])
  (:use [org.ego.core.common :only [properties gen-id]]
        [org.ego.xmpp.xmpp]))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(defn gen-resource
  [string]
  (if (nil? string)
    (str "Home_" (gen-id))
    (str string "_" (gen-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Process

(defmulti process (fn [content _] (-> content :attrs :type keyword)))

; TODO make sure this user is allowed to receive this message
(defmethod process :chat
  [content state]
   (do (let [msg (apply str (for [item (content :content)]
                              (if (= :body (:tag item))
                                (first (:content item)))))]
         (if (not (empty? msg))
           (log :info (str "says to " (:to (:attrs content)) " : " msg))))
       [(assoc-in (assoc-in content [:attrs :id] (gen-id))
                  [:attrs :from] (str (:username @state) "@" (properties :server:domain)))]))

(defmethod process :default
  [content state]
  (do (log :debug (str "sent unknown IQ " content))
      [{:tag :service-unavailable}]))