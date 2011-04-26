(ns boing.test.test-bean
  (:use [boing.bean] [boing.context] [clojure.test] [clojure.contrib.trace]))

(deftest test-beandef []
  (testing
    "Create bean definitions no args at all"
    (is (= (:java-class
             (defbean :test-bean-1 :boing.test.SimpleClass)) boing.test.SimpleClass))))

(deftest test-beandef []
  (testing
    "Create bean definitions using only constructors and keyword class names"
    (is (= (:java-class
             (defbean :test-bean-1 :boing.test.SimpleClass)) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 :boing.test.SimpleClass
               :c-args [(byte 1)])) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 :boing.test.SimpleClass
               :c-args [(byte 1) (short 2) (int 3) (long 4)])) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 :boing.test.SimpleClass
               :c-args [(byte 1) (short 2) (int 3) (long 4) "Test string"])) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 :boing.test.SimpleClass
               :c-args [(byte 1) (short 2) (int 3) (long 4) "Test string" (float 1.1) (double 1.2)])) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 :boing.test.SimpleClass
               :c-args [(byte 1) (short 2) (int 3) (long 4) "Test string" (float 1.1) (double 1.2) \H true])) boing.test.SimpleClass))))

(deftest test-beandef []
  (testing
    "Create bean definitions using only constructors and String class names"
    (is (= (:java-class
             (defbean :test-bean-1 "boing.test.SimpleClass")) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 "boing.test.SimpleClass"
               :c-args [(byte 1)])) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 "boing.test.SimpleClass"
               :c-args [(byte 1) (short 2) (int 3) (long 4)])) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 "boing.test.SimpleClass"
               :c-args [(byte 1) (short 2) (int 3) (long 4) "Test string"])) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 "boing.test.SimpleClass"
               :c-args [(byte 1) (short 2) (int 3) (long 4) "Test string" (float 1.1) (double 1.2)])) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 "boing.test.SimpleClass"
               :c-args [(byte 1) (short 2) (int 3) (long 4) "Test string" (float 1.1) (double 1.2) \H true])) boing.test.SimpleClass))))

(deftest test-beandef []
  (testing
    "Create bean definitions using only constructors: boing.test.SimpleClass"
    (is (= (:java-class
             (defbean :test-bean-1 boing.test.SimpleClass)) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 boing.test.SimpleClass
               :c-args [(byte 1)])) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 boing.test.SimpleClass
               :c-args [(byte 1) (short 2) (int 3) (long 4)])) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 boing.test.SimpleClass
               :c-args [(byte 1) (short 2) (int 3) (long 4) "Test string"])) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 boing.test.SimpleClass
               :c-args [(byte 1) (short 2) (int 3) (long 4) "Test string" (float 1.1) (double 1.2)])) boing.test.SimpleClass))
    (is (= (:java-class
             (defbean :test-bean-1 boing.test.SimpleClass
               :c-args [(byte 1) (short 2) (int 3) (long 4) "Test string" (float 1.1) (double 1.2) \H true])) boing.test.SimpleClass))))

(deftest test-beandef-options []
  (testing
    "Create bean definitions with some options"
    (let [simple-bean (defbean :test-bean-1 boing.test.SimpleClass :pre (fn [x] )
                        :post (fn [x] ) :class-override java.lang.Long)]
      (is (= (:class-override simple-bean) java.lang.Long)))))


(deftest test-new-bean-constructors []
  (testing
    "Instantiate beans using only constructors: boing.test.SimpleClass"
    (is (= (.toString
             (create-bean (defbean :test-bean-1 boing.test.SimpleClass))) "0:0:0:0:null:0.0:0.0:\\u0000:false"))
    (is (= (.toString
             (create-bean (defbean :test-bean-1 boing.test.SimpleClass
               :c-args [(byte 1)]))) "1:0:0:0:null:0.0:0.0:\\u0000:false"))
    (is (= (.toString
             (create-bean (defbean :test-bean-1 boing.test.SimpleClass
               :c-args [(byte 1) (short 2) (int 3) (long 4)]))) "1:2:3:4:null:0.0:0.0:\\u0000:false"))
    (is (= (.toString
             (create-bean (defbean :test-bean-1 boing.test.SimpleClass
               :c-args [(byte 1) (short 2) (int 3) (long 4) "Test String"]))) "1:2:3:4:Test String:0.0:0.0:\\u0000:false" ))))

(deftest test-new-bean-setters []
  (testing
    "Instantiate beans using an empty constructor and setters: boing.test.SimpleClass"
    (is (= (.toString
             (create-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :s-vals {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})))
          "1:2:3:4:This is a test:0.0:0.0:\\u0000:false"))
    (is (= (.toString
             (create-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :s-vals {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})))
          "0:0:0:0:null:2.3:3.4:H:true"))))

(deftest test-complex-defbean []
    (testing
      "Create bean definitions referring to other bean definitions"
      (let [first-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :s-vals {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            second-bean (defbean :test-bean-2 boing.test.SimpleClass
                          :s-vals {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})]
        (is (= (:java-class
                 (defbean :test-bean-2 boing.test.ComplexClass
                   :s-vals {:simpleBeanOne first-bean
                                :simpleBeanTwo second-bean})) boing.test.ComplexClass))))
    (testing
      "Create bean definitions referring to other anonymous bean definitions"
      (let [first-bean (defabean boing.test.SimpleClass
                         :s-vals {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            second-bean (defabean boing.test.SimpleClass
                          :s-vals {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})]
        (is (= (:java-class
                 (defbean :test-bean-2 boing.test.ComplexClass
                   :s-vals {:simpleBeanOne first-bean
                                :simpleBeanTwo second-bean})) boing.test.ComplexClass)))))
(deftest test-complex-instances []
    (testing
      "Instantiate bean referring to other beans"
      (let [first-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :s-vals {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            second-bean (defbean :test-bean-2 boing.test.SimpleClass
                          :s-vals {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})]
        (is (= (.toString
                 (create-bean (defbean :test-complex-bean-1 boing.test.ComplexClass
                   :s-vals {:simpleBeanOne first-bean
                                :simpleBeanTwo second-bean}))) "0:0:0:0:null:2.3:3.4:H:true:1:2:3:4:This is a test:0.0:0.0:\\u0000:false"))))
    (testing
      "Instantiate beans referring to other anonymous beans"
      (let [first-bean (defabean boing.test.SimpleClass
                         :s-vals {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            second-bean (defabean boing.test.SimpleClass
                          :s-vals {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})]
        (is (= (.toString
                 (create-bean (defbean :test-bean-2 boing.test.ComplexClass
                                :s-vals {:simpleBeanOne first-bean
                                :simpleBeanTwo second-bean}))) "0:0:0:0:null:2.3:3.4:H:true:1:2:3:4:This is a test:0.0:0.0:\\u0000:false")))))

(deftest test-singleton []
    (testing
      "Instantiate singleton"
      (let [singleton (defbean :test-bean-1 boing.test.SimpleClass :mode :singleton
                         :s-vals {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            alien-singleton 
            (with-context :my-ctx
              (defbean :test-bean-1 boing.test.SimpleClass :mode :singleton
                :s-vals {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true}))]
        (is (= (.hashCode (create-bean singleton)) (.hashCode (create-bean singleton))))
        (with-context :my-ctx
          (is (= (.hashCode (create-bean alien-singleton)) (.hashCode (create-bean alien-singleton)))))
        )))

(deftest test-beanref []
    (testing
      "Testing bean references"
      (let [first-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :s-vals {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            second-bean (defbean :test-bean-2 boing.test.SimpleClass
                          :s-vals {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})]
        (is (= (.toString
                 (create-bean (defbean :test-complex-bean-1 boing.test.ComplexClass
                                :s-vals {:simpleBeanOne :test-bean-1 :simpleBeanTwo :test-bean-2})))
              "0:0:0:0:null:2.3:3.4:H:true:1:2:3:4:This is a test:0.0:0.0:\\u0000:false"))
        (is (= (.toString
                 (create-bean (defbean :test-complex-bean-1 boing.test.ComplexClass
                                :s-vals {:simpleBeanOne :test-bean-1 :simpleBeanTwo :test-bean-2})))
              "0:0:0:0:null:2.3:3.4:H:true:1:2:3:4:This is a test:0.0:0.0:\\u0000:false")))))

(deftest test-closures []
    (testing
      "Test closures"
      (let [dbl 7.8
            nb 5
            first-bean (defabean boing.test.SimpleClass
                         :s-vals {:intVal (fn [] nb) :floatVal (float 2.3) :doubleVal (fn [] dbl) :charVal \H :boolVal true})]
        (is (= (.toString (create-bean first-bean)) "0:0:5:0:null:2.3:7.8:H:true")))))

(deftest test-maps-and-lists []
    (testing
      "Test maps and lists"
      (let [listval (list 1 2 3)
            mapval { :a 1 :b 2}
            first-bean (defabean boing.test.SimpleClass
                         :s-vals {:listVal listval :mapVal mapval})]
        (is (= (.toString (create-bean first-bean)) "0:0:0:0:null:0.0:0.0:\\u0000:false:(1 2 3):{:a 1, :b 2}")))))