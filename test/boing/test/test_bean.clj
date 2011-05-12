(ns boing.test.test-bean
  (:use [boing.bean] [boing.context] [clojure.test] [clojure.contrib.trace]))

(deftest test-noargs-beandef []
  (testing
    "Testing bean definitions without args"
    (is (= (:java-class
             (defbean :test-bean-1 :boing.test.SimpleClass)) boing.test.SimpleClass))))

(deftest test-class-constructor-beandef []
  (testing
    "Testing bean definitions using only constructors and keyword class names"
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

(deftest test-string-constructor-beandef []
  (testing
    "Testing bean definitions using only constructors and String class names"
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

(deftest test-constructor-properties []
  (testing
    "Testing bean definitions using only constructors: boing.test.SimpleClass"
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

(deftest test-constructor-w-options []
  (testing
    "Testing bean definitions with some options"
    (let [simple-bean (defbean :test-bean-1 boing.test.SimpleClass :pre (fn [x] )
                        :post (fn [x] ) :class-override java.lang.Long)]
      (is (= (:class-override simple-bean) java.lang.Long)))))


(deftest test-create-bean-constructors []
  (testing
    "Testing bean creationusing only constructors: boing.test.SimpleClass"
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

(deftest test-create-bean-w-setters []
  (testing
    "Testing bean creation using an empty constructor and setters: boing.test.SimpleClass"
    (is (= (.toString
             (create-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :s-vals {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})))
          "1:2:3:4:This is a test:0.0:0.0:\\u0000:false"))
    (is (= (.toString
             (create-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :s-vals {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})))
          "0:0:0:0:null:2.3:3.4:H:true"))))

(deftest test-create-complex-w-setters[]
    (testing
      "Testing bean creation referring to other beans"
      (let [first-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :s-vals {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            second-bean (defbean :test-bean-2 boing.test.SimpleClass
                          :s-vals {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})]
        (is (= (:java-class
                 (defbean :test-bean-2 boing.test.ComplexClass
                   :s-vals {:simpleBeanOne first-bean
                                :simpleBeanTwo second-bean})) boing.test.ComplexClass))))
    (testing
      "Testing beans creation referring to other anonymous beans"
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
      "Testing bean creation referring to other beans"
      (let [first-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :s-vals {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            second-bean (defbean :test-bean-2 boing.test.SimpleClass
                          :s-vals {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})]
        (is (= (.toString
                 (create-bean (defbean :test-complex-bean-1 boing.test.ComplexClass
                   :s-vals {:simpleBeanOne first-bean
                                :simpleBeanTwo second-bean}))) "0:0:0:0:null:2.3:3.4:H:true:1:2:3:4:This is a test:0.0:0.0:\\u0000:false"))))
    (testing
      "Testing beans creation referring to other anonymous beans"
      (let [first-bean (defabean boing.test.SimpleClass
                         :s-vals {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            second-bean (defabean boing.test.SimpleClass
                          :s-vals {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})]
        (is (= (.toString
                 (create-bean (defbean :test-bean-2 boing.test.ComplexClass
                                :s-vals {:simpleBeanOne first-bean
                                :simpleBeanTwo second-bean}))) "0:0:0:0:null:2.3:3.4:H:true:1:2:3:4:This is a test:0.0:0.0:\\u0000:false")))))

(deftest test-singleton-setters []
    (testing
      "Testing singleton creation using setters"
      (let [singleton (defbean :test-bean-1 boing.test.SimpleClass :mode :singleton
                         :s-vals {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            alien-singleton 
              (defbean :test-bean-1 boing.test.SimpleClass :mode :singleton
                :s-vals {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})]
        (is (= (.hashCode (create-bean singleton)) (.hashCode (create-bean singleton))))
        (with-context :my-ctx
          (is (= (.hashCode (create-bean alien-singleton)) (.hashCode (create-bean alien-singleton)))))
        )))

(deftest test-singleton-constructor []
    (testing
      "Testing singleton creation using constructor"
      (let [singleton (defbean :test-bean-1 boing.test.SimpleClass :mode :singleton
                         :c-args [(byte 1) (short 2) (int 3) (long 4) "Test string" (float 1.1) (double 1.2) \H true])
            alien-singleton 
            (with-context :my-ctx
              (defbean :test-bean-1 boing.test.SimpleClass :mode :singleton
                :c-args [(byte 1) (short 2) (int 3) (long 4) "Test string" (float 1.1) (double 1.2) \H true]))]
        (is (= (.hashCode (create-bean singleton)) (.hashCode (create-bean singleton))))
        (with-context :my-ctx
          (is (= (.hashCode (create-bean alien-singleton)) (.hashCode (create-bean alien-singleton)))))
        )))


(deftest test-setters-beanref []
    (testing
      "Testing bean creation with bean references using setters"
      (let [first-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :s-vals {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            second-bean (defbean :test-bean-2 boing.test.SimpleClass
                          :s-vals {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})]
        (is (= (.toString
                 (create-bean (defbean :test-complex-bean-1 boing.test.ComplexClass
                                :s-vals {:simpleBeanOne :test-bean-1 :simpleBeanTwo :test-bean-2})))
              "0:0:0:0:null:2.3:3.4:H:true:1:2:3:4:This is a test:0.0:0.0:\\u0000:false")))))


(deftest test-constructor-beanref []
    (testing
      "Testing bean creation with bean references using constructor"
      (let [first-bean (defbean :test-bean-1 boing.test.SimpleClass
                         :s-vals {:floatVal (float 2.3) :doubleVal (double 3.4) :charVal \H :boolVal true})
            second-bean (defbean :test-bean-2 boing.test.SimpleClass
                          :s-vals {:byteVal (byte 1) :shortVal (short 2) :intVal (int 3) :longVal (long 4) :stringVal "This is a test"})]
        (is (= (.toString
                 (create-bean (defbean :test-complex-bean-1 boing.test.ComplexClass
                                :c-args [:test-bean-1 :test-bean-2])))
              "0:0:0:0:null:2.3:3.4:H:true:1:2:3:4:This is a test:0.0:0.0:\\u0000:false")))))

(deftest test-setters-closures []
    (testing
      "Test bean creation with closures using setters"
      (let [dbl 7.8
            nb 5
            first-bean (defabean boing.test.SimpleClass
                         :s-vals {:intVal (fn [] nb) :floatVal (float 2.3) :doubleVal (fn [] dbl) :charVal \H :boolVal true})]
        (is (= (.toString (create-bean first-bean)) "0:0:5:0:null:2.3:7.8:H:true")))))

(deftest test-constructor-closures []
    (testing
      "Test bean creation with closures using constructors"
      (let [dbl 7.8
            nb 5
            first-bean (defabean boing.test.SimpleClass
                         :c-args [(byte 0) (short 0) (fn [] nb) (long 0) "test" (float 2.3) (fn [] dbl) \H  true])]
        (is (= (.toString (create-bean first-bean)) "0:0:5:0:test:2.3:7.8:H:true")))))

(deftest test-setters-maps-and-lists []
    (testing
      "Test bean creation with maps and lists using setters"
      (let [listval (list 1 2 3)
            mapval { :a 1 :b 2}
            first-bean (defabean boing.test.SimpleClass
                         :s-vals {:listVal listval :mapVal mapval})]
        (is (= (.toString (create-bean first-bean)) "0:0:0:0:null:0.0:0.0:\\u0000:false:(1 2 3):{:a 1, :b 2}")))))


(deftest test-constructors-maps-and-lists []
    (testing
      "Test bean creation with maps and lists using constructors"
      (let [listval (list 1 2 3)
            mapval { :a 1 :b 2}
            first-bean (defabean boing.test.SimpleClass
                         :c-args [ listval mapval])]
        (is (= (.toString (create-bean first-bean)) "0:0:0:0:null:0.0:0.0:\\u0000:false:(1 2 3):{:a 1, :b 2}")))))


(deftest test-setters-properties-class []
    (testing
      "Test bean creation with Properties args using setters"
      (let [mapval { :a 1 :b 2}
            first-bean (defabean boing.test.SimpleClass
                         :s-vals {:props mapval})]
        (is (= (.toString (create-bean first-bean)) "0:0:0:0:null:0.0:0.0:\\u0000:false:{b=2, a=1}")))))


(deftest test-constructors-properties-class []
    (testing
      "Test bean creation with Properties args using constructors"
      (let [mapval { :a 1 :b 2}
            first-bean (defabean boing.test.SimpleClass
                         :c-args [ mapval])]
        (is (= (.toString (create-bean first-bean)) "0:0:0:0:null:0.0:0.0:\\u0000:false:{b=2, a=1}")))))


