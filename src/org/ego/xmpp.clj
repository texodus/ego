(ns org.ego.xmpp
  (:gen-class)
  (:import [java.net InetAddress InetSocketAddress URL]
           [java.io InputStreamReader OutputStreamWriter PushbackReader ByteArrayInputStream Reader Writer OutputStream FileInputStream]
           [java.util.concurrent Executors]
           [java.security KeyStore Security]
           [java.security.cert X509Certificate]
           [javax.net.ssl SSLContext KeyManagerFactory TrustManager SSLEngine]
           [org.jboss.netty.bootstrap ServerBootstrap]
           [org.jboss.netty.channel Channel SimpleChannelHandler ChannelFutureListener ChannelHandlerContext ChannelStateEvent ChildChannelStateEvent ExceptionEvent UpstreamMessageEvent DownstreamMessageEvent]
           [org.jboss.netty.channel.socket.nio NioServerSocketChannelFactory]
           [org.jboss.netty.handler.ssl SslHandler]
           [org.jboss.netty.handler.codec.string StringEncoder StringDecoder]
           [org.jboss.netty.handler.codec.frame Delimiters DelimiterBasedFrameDecoder]
           [org.jboss.netty.logging InternalLoggerFactory Log4JLoggerFactory])
  (:require [org.ego.core.common :as common])
  (:use [clojure.contrib.logging :only [log]]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Channel 

(deftype Connection [#^ChannelHandlerContext context #^Channel channel] clojure.lang.IPersistentMap) 

(def #^{:private true} connections (ref {}))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Server

(defprotocol Server
  "Handles the abstraction of a pipeline handler"
  (upstream [x ip msg])
  (downstream [x ip msg])
  (connect [x ip])
  (disconnect [x ip]))

(defn- get-channel-handler
  "Generates a netty ChannelHandler given a shared ref to a channel-buffer;  a
   channel-buffer ref is necessary to pass characters from the netty messageReceived
   to the sax parser's InputStream"
  [server] 
  (proxy [SimpleChannelHandler] []
    (messageReceived 
     [#^ChannelHandlerContext ctx #^UpstreamMessageEvent me]
     (let [msgs (.upstream server (.. me getChannel getRemoteAddress) (. me getMessage))]
      (if (not (empty? msgs))
         (doseq [message msgs]
           (. ctx (sendUpstream (new UpstreamMessageEvent 
                                     (. me getChannel) 
                                     message
                                     (.. me getChannel getRemoteAddress))))))))
    (writeRequested
     [#^ChannelHandlerContext ctx #^DownstreamMessageEvent me]
     (let [msg (.downstream server (.. me getChannel getRemoteAddress) (. me getMessage))]
       (if (not (nil? msg))
         (. ctx (sendDownstream (new DownstreamMessageEvent 
                                     (. me getChannel) 
                                     (. me getFuture)
                                     msg 
                                     (.. me getChannel getRemoteAddress)))))))
    (channelConnected 
     [#^ChannelHandlerContext ctx #^ChannelStateEvent event] 
     (do (.connect server (.. event getChannel getRemoteAddress)))
         (. ctx (sendUpstream event)))
    (channelDisconnected
     [#^ChannelHandlerContext ctx #^ChannelStateEvent event] 
     (do (.disconnect server (.. event getChannel getRemoteAddress)))
         (. ctx (sendUpstream event)))))

(def #^{:private true} base-server-handler
     (proxy [SimpleChannelHandler] []
       (messageReceived 
        [#^ChannelHandlerContext ctx #^UpstreamMessageEvent me]
        (.sendUpstream ctx me))
       (writeRequested
        [#^ChannelHandlerContext ctx #^DownstreamMessageEvent me]
        (.sendDownstream ctx me))
       (channelConnected 
        [#^ChannelHandlerContext ctx #^ChannelStateEvent cse] 
        (do (dosync (alter connections assoc (.. cse getChannel getRemoteAddress) (Connection ctx (.getChannel cse)))) 
            (log :info (str "Channel " (.. cse getChannel getRemoteAddress) " Connected"))
            (. ctx (sendUpstream cse))))
       (channelDisconnected
        [#^ChannelHandlerContext ctx #^ChannelStateEvent cse] 
        (do (dosync (alter connections dissoc (.. cse getChannel getRemoteAddress)))
            (log :info (str "Channel " (.. cse getChannel getRemoteAddress) " Disconnected"))
            (. ctx (sendUpstream cse))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; SSL/TLS

(defn- get-ssl-context
  "Generate an SSLContext for a KeyStore"
  [path key-pass cert-pass]
  (doto (. SSLContext (getInstance "TLS"))
    (.init (.getKeyManagers (doto (. KeyManagerFactory (getInstance "SunX509"))
                              (.init (doto (KeyStore/getInstance "JKS")
                                       (.load (. ClassLoader (getSystemResourceAsStream path)) 
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
;;;;
;;;; Provides functions to start and stop a server, and also implements netty
;;;; level functionality like TLS

(defn start-tls
  "Switch channel to TLS"
  [ip]
  (let [conn    (@connections ip)
        handler (get-ssl-handler (common/properties :server:keystore)
                                 (common/properties :server:keypassword)
                                 (common/properties :server:certificatepassword))]
    (log :debug (str "Starting SSL Handshake "))
    (do (.. (:context conn) getPipeline (addFirst "ssl" handler))
        (.. handler 
            (handshake (:channel conn))
            (addListener (proxy [ChannelFutureListener] []
                           (operationComplete 
                            [future]
                            (log :debug (str "SSL Handshake finished : " (. future isSuccess))))))))))

(defn close-channel
  [ip]
  (do (.close (:channel (@connections ip)))
      nil))

(defn channel-write
  [ip msg]
  (.write (:channel (@connections ip)) msg))

(defn start-server
  "Create a new socket server bound to the port, adding the supplied funs 
   in order to the default pipeline"
  [port & servers]
  (do (. InternalLoggerFactory (setDefaultFactory (Log4JLoggerFactory.)))
      (let [bootstrap (new ServerBootstrap 
                           (new NioServerSocketChannelFactory
                                (. Executors newCachedThreadPool)
                                (. Executors newCachedThreadPool)))]
        (do (let [pipeline (. bootstrap getPipeline)]
              (doto pipeline
                (.addLast "decoder" (StringDecoder.))
                (.addLast "encoder" (StringEncoder.))
                (.addLast "ego-base" base-server-handler))
              (doseq [s servers]
                (. pipeline addLast (str "handler_" (.toString s)) (get-channel-handler s))))
            (. bootstrap (setOption "child.tcpNoDelay" true))
            (. bootstrap (setOption "child.keepAlive" true))
            (. bootstrap (bind (InetSocketAddress. port)))))))

(defn stop-server
  "Stop the supplied server"
  [server]
  (. server close))


