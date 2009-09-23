(ns org.ego-test
  (:require [org.ego.xmpp.stanza-test]
            [org.ego.xmpp.xml-test])
  (:use [clojure.contrib.test-is]))
   
(run-tests 'org.ego.xmpp.stanza-test 'org.ego.xmpp.xml-test)