(defproject ego "0.1-SNAPSHOT" 
  :description "Distributed Social CMS" 
  :repositories [["Grizzly" "http://download.java.net/maven/2"]
                 ["JBoss" "http://repository.jboss.org/maven2"]]
  :dependencies [[org.clojure/clojure "1.1.0-new-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.0-SNAPSHOT"]
                 [org.clojars.dvgb/compojure "0.3.1"]
                 [org.glassfish.grizzly/grizzly-framework "2.0.0-SNAPSHOT"]
                 [commons-codec/commons-codec "1.3"]
                 [org.jboss.netty/netty "3.1.0.BETA2"]
                 [redis-clojure "1.0-SNAPSHOT"]
                 [log4j/log4j "1.2.14"]
                 [postgresql/postgresql "8.3-603.jdbc3"]]
  :dev-dependencies [[leiningen/lein-swank "1.0.0-SNAPSHOT"]]
  :main org.ego)