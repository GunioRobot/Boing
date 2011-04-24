(ns boing.test.test-resource
  (:use [boing.bean] [boing.resource] [clojure.test]))

(deftest test-load-properties []
  (testing "Load a property file either as a file or on the classpath"
    (is (= (load-properties "test.properties")
           {:prop4 "4", :prop3 "3", :prop2 "2", :prop1 "1"}))))