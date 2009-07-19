(ns org.ego.server
  (:gen-class)
  (:import [java.net InetAddress InetSocketAddress URL]
           [java.io InputStreamReader OutputStreamWriter PushbackReader ByteArrayInputStream Reader Writer OutputStream FileInputStream]
           [java.util.concurrent Executors]
           [java.security KeyStore Security]
           [java.security.cert X509Certificate]
           [javax.net.ssl SSLContext KeyManagerFactory TrustManager SSLEngine]
           [org.jboss.netty.bootstrap ServerBootstrap]
           [org.jboss.netty.channel SimpleChannelHandler ChannelFutureListener]
           [org.jboss.netty.channel.socket.nio NioServerSocketChannelFactory]
           [org.jboss.netty.handler.ssl SslHandler]
           [org.jboss.netty.handler.codec.string StringEncoder StringDecoder]
           [org.jboss.netty.handler.codec.frame Delimiters DelimiterBasedFrameDecoder]
           [org.jboss.netty.logging InternalLoggerFactory Log4JLoggerFactory]
           [org.apache.log4j Logger])
  (:require [clojure.xml :as xml]
            [org.ego.config :as config]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(def #^{:private true} conf (config/get-properties "server"))
(def #^{:private true} log (. Logger (getLogger (str *ns*))))

; ref map for tracking current connections - maps Connections to connection-records
(def #^{:private true} connections (ref {}))

(defstruct connection-record :buffer :channel :pipeline)

(def *connection*)

(defn- on-thread [f]
  "Create a new thread to run f"
  (doto (Thread. #^Runnable f)
    (.start)))

(defn- strip-lines
  "Remove newlines from a string"
  [text]
  (apply str (filter #(not (or (= (int %) 13) (= (int %) 10))) text)))

(defmulti #^{:private true} channel-write 
  "Helper function for writing various messages types to a Channel"
  (fn [x _] (if (.isArray (class x)) :array (class x))))
(defmethod channel-write java.lang.Integer
  [channel val]
  (. channel (write (str (char val)))))
(defmethod channel-write :array
  [channel val]
  (. channel (write (cast String (apply str val)))))
(defmethod channel-write :default
  [channel val]
  (. channel (write (str val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Streams

(defn- get-chars
  "Get chars from the buffer and remove them in one transaction"
  [connection off len]
  (dosync (let [temp-chars (if (= :close (-> @connection :buffer first))
                             [:close]
                             (apply str (doall (take len (drop off (:buffer @connection))))))]
            (alter connection assoc
                   :buffer (drop (+ off len) (:buffer @connection)))
            temp-chars)))


(defn- get-channel-in-reader
  "Creates a Reader from the netty Channel for reading by sax.  Blocks until
   characters are available in the buffer, then modifies provided array and
   returns the length."
  [connection]
  (proxy [Reader] []
    (read [chars off len]
          (let [char-array (loop [return-chars (get-chars connection off len)]
                             (if (zero? (count return-chars))
                               (do (. Thread (sleep 300))
                                   (recur (get-chars connection off len)))
                               (do (. log (debug (str "Received from " 
                                                      (. (:channel @connection) getRemoteAddress) 
                                                      " : " return-chars)))
                                   return-chars)))]
            (loop [ret-chars char-array, index 0]
              (if (zero? (count ret-chars))
                (count char-array)
                (if (= (first ret-chars) :close)
                  -1 ; reached end of stream
                  (do (aset chars index (first ret-chars))
                      (recur (rest ret-chars) (inc index))))))))
    (close [] nil)))

(defn- get-channel-out-writer
  "Creates a Writer from the netty Channel for writing by the sax handler."
  [connection]
  (let [write-buffer (ref "")]
    (proxy [Writer] []
      (write [val]
             (dosync (alter write-buffer str val)))
      (flush []
             (do (. log (debug (str "Flushed to " (.getRemoteAddress (:channel @connection)) " : " @write-buffer)))
                 (. (:channel @connection)
                    (write (dosync (let [text @write-buffer]
                                     (ref-set write-buffer "")
                                     text))))))
      (close [] nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Handlers

(defn get-ip
  "Return the IP for the current connection"
  []
  (. (:channel @*connection*) getRemoteAddress))

(defn open-channel
  [fun channel pipeline]
  (let [connection (ref (struct connection-record [] channel pipeline))]
    (do (dosync (alter connections assoc channel connection))
        (on-thread #(binding [*out* (get-channel-out-writer connection)
                              *in* (get-channel-in-reader connection)
                              *connection* connection]
                      (fun)))
        (. log (info (str "Channel connected : " (. channel getRemoteAddress)))))))

(defn close-channel
  "test"
  ([] (close-channel (:channel *connection*)))
  ([channel] (do (dosync (alter (@connections channel) assoc :buffer [:close])
                         (alter connections dissoc channel))
                 (. channel close)
                 (. log (info (str "Channel disconnected : " (. channel getRemoteAddress)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Handlers

(defn- get-channel-handler
  "Generates a netty ChannelHandler given a shared ref to a channel-buffer;  a
   channel-buffer ref is necessary to pass characters from the netty messageReceived
   to the sax parser's InputStream"
  [fun] 
  (proxy [SimpleChannelHandler] []
    (messageReceived 
     [ctx event]
     (let [msg (strip-lines (. event getMessage))
           connection (@connections (. event getChannel))]
       (dosync (alter connection assoc :buffer (conj (:buffer @connection) msg)))))
    (channelConnected [ctx event] (open-channel fun (. event getChannel) (. ctx getPipeline)))
    (channelDisconnected [ctx event] (close-channel (. event getChannel)))))


(defn- get-ssl-context
  "Generate an SSLContext for a KeyStore"
  [path key-pass cert-pass]
  (doto (. SSLContext (getInstance "TLS"))
    (.init (.getKeyManagers (doto (. KeyManagerFactory (getInstance "SunX509"))
                              (.init (doto (KeyStore/getInstance "JKS")
                                       (.load (FileInputStream. path) 
                                              (.toCharArray key-pass)))
                                     (.toCharArray cert-pass))))
           (into-array [(proxy [TrustManager] []
                          (getAcceptedIssuers [] (make-array X509Certificate 0))
                          (checkClientTrusted [x y] nil)
                          (checkServerTrusted [x y] nil))])
           nil)))

(defn- get-ssl-handler
  "Generate an SSLHandler"
  [path key-pass cert-pass]
  (SslHandler. (doto (.createSSLEngine (get-ssl-context path key-pass cert-pass))
                 (.setUseClientMode false))
               true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Public

(defn start-tls
  "Modify channel pipeline to utilize TLS"
  []
  (let [handler (get-ssl-handler (conf :keystore)
                                 (conf :keypassword)
                                 (conf :certificatepassword))]
    (. log (debug (str "Starting SSL Handshake for " (.getRemoteAddress (:channel @*connection*)))))
    (do (doto (:pipeline @*connection*)
          (.addFirst "ssl" handler))
        (.. handler 
            (handshake (:channel @*connection*)) 
            (addListener (proxy [ChannelFutureListener] []
                           (operationComplete 
                            [future]
                            (do (. log (debug (str "SSL Handshake finished : " (. future isSuccess))))))))))))

(defn get-ip
  "Return the IP for the current connection"
  []
  (. (:channel @*connection*) getRemoteAddress))

(defn create-server
  "Create a new socket server bound to the port"
  [port fun]
  (do (. InternalLoggerFactory (setDefaultFactory (Log4JLoggerFactory.)))
      (let [bootstrap (ServerBootstrap. (NioServerSocketChannelFactory. (. Executors newCachedThreadPool)
                                                                        (. Executors newCachedThreadPool)))]
        (do (doto (. bootstrap getPipeline)
              (.addLast "decoder" (StringDecoder.))
              (.addLast "encoder" (StringEncoder.))
              (.addLast "handler" (get-channel-handler fun)))
            (. bootstrap (setOption "child.tcpNoDelay" true))
            (. bootstrap (setOption "child.keepAlive" true))
            (. bootstrap (bind (InetSocketAddress. port)))))))

(defn stop-server
  "Stop the supplied server"
  [server]
  (. server close))