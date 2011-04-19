(ns boing.test.test-bean
  (:use [boing.bean] [boing.core.types] [clojure.test])
  (:import []))

(deftest test-beandef []
  (testing "Create bean definitions using only constructors: boing.test.SimpleClass"
    (is (= (:java-class
             (defbean :test-bean-1 boing.test.SimpleClass)) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 boing.test.SimpleClass
               :constructor-args [(byte 1)])) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 boing.test.SimpleClass
               :constructor-args [(byte 1) (short 2) (int 3) (long 4)])) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 boing.test.SimpleClass
               :constructor-args [(byte 1) (short 2) (int 3) (long 4) "Test string"])) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 boing.test.SimpleClass
               :constructor-args [(byte 1) (short 2) (int 3) (long 4) "Test string" (float 1.1) (double 1.2)])) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 boing.test.SimpleClass
               :constructor-args [(byte 1) (short 2) (int 3) (long 4) "Test string" (float 1.1) (double 1.2) \H true])) boing.test.SimpleClass))))

(deftest test-new-bean-constructors []
  (testing "Instantiate beans using only constructors: boing.test.SimpleClass"
    (is (= (.toString
             (create-bean (defbean :test-bean-1 boing.test.SimpleClass))) "0:0:0:0:null:0.0:0.0:\\u0000:false"))
    (is (= (.toString
             (create-bean (defbean :test-bean-1 boing.test.SimpleClass
               :constructor-args [(byte 1)]))) "1:0:0:0:null:0.0:0.0:\\u0000:false"))
    (is (= (.toString
             (create-bean (defbean :test-bean-1 boing.test.SimpleClass
               :constructor-args [(byte 1) (short 2) (int 3) (long 4)]))) "1:2:3:4:null:0.0:0.0:\\u0000:false"))
    (is (= (.toString
             (create-bean (defbean :test-bean-1 boing.test.SimpleClass
               :constructor-args [(byte 1) (short 2) (int 3) (long 4) "Test String"]))) "1:2:3:4:Test String:0.0:0.0:\\u0000:false" ))))

(deftest test-new-bean-setters []
  (testing "Instantiate beans using an empty constructor and setters: boing.test.SimpleClass"
    (is (= (.toString
             (create-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :properties {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})))
          "1:2:3:4:This is a test:0.0:0.0:\\u0000:false"))
    (is (= (.toString
             (create-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :properties {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})))
          "0:0:0:0:null:2.3:3.4:H:true"))))

(deftest test-complex-defbean []
    (testing "Create bean definitions referring to other bean definitions"
      (let [first-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :properties {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            second-bean (defbean :test-bean-1 boing.test.SimpleClass
                          :properties {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})]
        (is (= (:java-class
                 (defbean :test-bean-2 boing.test.ComplexClass
                   :properties {:simpleBeanOne first-bean
                                :simpleBeanTwo second-bean})) boing.test.ComplexClass))))
    (testing "Create bean definitions referring to other anonymous bean definitions"
      (let [first-bean (defabean boing.test.SimpleClass
                         :properties {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            second-bean (defabean boing.test.SimpleClass
                          :properties {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})]
        (is (= (:java-class
                 (defbean :test-bean-2 boing.test.ComplexClass
                   :properties {:simpleBeanOne first-bean
                                :simpleBeanTwo second-bean})) boing.test.ComplexClass)))))
(deftest test-complex-instances []
    (testing "Instantiate bean referring to other beans"
      (let [first-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :properties {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            second-bean (defbean :test-bean-1 boing.test.SimpleClass
                          :properties {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})]
        (is (= (.toString
                 (create-bean (defbean :test-complex-bean-1 boing.test.ComplexClass
                   :properties {:simpleBeanOne first-bean
                                :simpleBeanTwo second-bean}))) "0:0:0:0:null:2.3:3.4:H:true:1:2:3:4:This is a test:0.0:0.0:\\u0000:false"))))
    (testing "Instantiate beans referring to other anonymous beans"
      (let [first-bean (defabean boing.test.SimpleClass
                         :properties {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            second-bean (defabean boing.test.SimpleClass
                          :properties {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})]
        (is (= (.toString
                 (create-bean (defbean :test-bean-2 boing.test.ComplexClass
                   :properties {:simpleBeanOne first-bean
                                :simpleBeanTwo second-bean}))) "0:0:0:0:null:2.3:3.4:H:true:1:2:3:4:This is a test:0.0:0.0:\\u0000:false")))))

(deftest test-singleton []
    (testing "Instantiate singleton"
      (let [singleton (defbean :test-bean-1 boing.test.SimpleClass :mode :singleton
                         :properties {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})]
        (is (= (.hashCode (create-bean singleton)) (.hashCode (create-bean singleton)))))))

