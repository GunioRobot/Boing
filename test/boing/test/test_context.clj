(ns boing.test.test-context
    (:use [boing.bean] [boing.context] [clojure.test]))

(deftest test-default-context []
  (testing "Testing default bean context"
    (let [first-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :properties {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
          second-bean (defbean :test-bean-2 boing.test.SimpleClass
                          :properties {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})]      
      (is (= (.hashCode first-bean) (.hashCode (find-beandef :test-bean-1))))
      (is (= first-bean (find-beandef :test-bean-1)))
      (is (= (get-bean-ids) [:test-bean-1 :test-bean-2])))))

(deftest test-arbitrary-context []
  (testing "Testing default bean context"
    (with-context :alternate
      (let [first-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :properties {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            second-bean (defbean :test-bean-2 boing.test.SimpleClass
                          :properties {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})]      
        (is (= (.hashCode first-bean) (.hashCode (find-beandef :test-bean-1))))
        (is (= first-bean (find-beandef :test-bean-1)))
        (is (= (get-bean-ids) [:test-bean-1 :test-bean-2])))
        (is (= *current-context* :alternate)))))