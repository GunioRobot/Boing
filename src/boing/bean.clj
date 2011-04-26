(ns boing.bean
  "Implement bean definitions and instanciations.
   Beans can be defined using defbean or defabean.
   Bean definitions trigger a search for constructors and setters of the given class.
   Instanciation of a bean can be done using create-bean on a bean definition."
  (:use
    [boing.core.reflector] [boing.context] [clojure.stacktrace]
    [clojure.contrib.def]
    [clojure.contrib.trace])
  (:import [java.lang.reflect Modifier] [boing Util]))

(declare allocate-bean)

(defrecord Bean [context id java-class mode setters constructor
                 c-args init pre post class-override comments])

(defprotocol BoingBean
  "Unifies a few methods between bean/non-bean objects."
  (get-value [this])
  (get-class [this]))

(extend-type Bean
  BoingBean
  (get-value [this] (allocate-bean this))
  (get-class [this] (let [{:keys [java-class class-override]} this]
                      (if (nil? class-override) java-class class-override))))

(extend-type clojure.lang.Keyword
  BoingBean
  (get-value [this]
    (if-let [beandef (find-beandef this)] ;; We need to fetch the bean definition from the current context.
      (allocate-bean beandef)             ;; and allocate it
      (throw (Exception. (format "Cannot find bean %s in context %s" this *current-context*)))))
  (get-class [this] clojure.lang.Keyword))

(extend-type clojure.lang.Fn
  BoingBean
  (get-value [this]
    (try
      (this)               ;; We need to call the closure to obtain the value to assign
      (catch Exception e# (throw e#))))
  (get-class [this] clojure.lang.AFn))

(extend-type Object        ;; "Default' implementation
  BoingBean
  (get-value [this] this)
  (get-class [this] (.getClass this)))

(defvar- *singletons* (atom {}))

(defn- singleton-name
  ([id]
    (keyword (str "singleton-" id (name *current-context*)))))

(defn- singleton?
  "Check if the given bean id corresponds to a singleton in the cache for the current context.
   if yes returns it. Singletons are not context dependent."
  [beandef]
  (cond
    (= (:mode beandef) :singleton) (get @*singletons* (singleton-name (:id beandef)))
    :else nil))

(defn- register-singleton
  "Register a singleton in the cache in the current context."
  [id instance]
    (swap! *singletons* #(merge %1 %2) { (singleton-name id) instance}))
 

(defn- valid-setter-sig?
  "Validate if a setter's argument class matches its value class.
   If the setter refers to another bean definition, validate with the class in the definition
   since the real object is not yet instanciated.
   If the value is a closure, accept it as is, class cast exception will be trapped at run time.
   If a validation fails, we throw an exception otherwise return true."
  [setter value]
  (let [{:keys [mth-name mth-arg-class]} (meta setter)
        value-class (Util/getPrimitiveClass (get-class value))]
      (cond
        (= value-class clojure.lang.AFn)
        true
        (= value-class clojure.lang.Keyword) true
        (= value-class clojure.lang.PersistentList) true
        (= value-class clojure.lang.PersistentArrayMap) true
        (= value-class clojure.lang.PersistentHashMap) true
        (not (= value-class mth-arg-class))
        (throw (Exception. (format "Setter %s: argument class mismatch, got %s, expecting %s" mth-name value-class mth-arg-class)))
        :else true)))

(defn- valid-bean-setters?
  "Checks if a bean has all the necessary property setters
   for the given property map"
  [java-class properties]
  (let [setters (find-all-setters java-class)]
    (into {} (doall (map #(let [setter(% setters)
                                property (% properties)]
                            (do
                              (cond (nil? (% setters))
                                (throw (Exception. (format "No setter for property %s in class %s" % java-class)))
                                (not (nil? property))
                                (do
                                  (valid-setter-sig? setter property)                                  
                                  { % {:setter setter :property % :value  (to-java-arg property (:mth-arg-class (meta setter)))}})
                                :else {}))) (keys properties))))))
(defn- to-class
  "Classes in bean definitions can be expressed as classes, keywords or strings.
   This function normalizes these to Class objects."
  [java-class]
  (try
	  (cond
	    (class? java-class) java-class
	    (keyword?  java-class)
	    (java.lang.Class/forName (name java-class))
	    (instance? java.lang.String java-class)
	    (java.lang.Class/forName (name java-class))
	    :else
	    (throw (Exception. (format "Cannot get a class from %s" java-class))))
   (catch ClassNotFoundException e# (throw (Exception. (format "Class not found exception: %s" java-class))))
   (catch Exception e# (throw (Exception. (format "Error loading class %s: %s" java-class (.getMessage e#)))))))

(defn defbean
  "Create a bean definition"
  ([bean-name jclass & {:keys [mode s-vals c-args init pre post class-override comments] :or {mode :prototype}}]
  (try
      (let [id (keyword bean-name)
            java-class (to-class jclass)
            setters (valid-bean-setters? java-class s-vals)
            constructor (valid-constructor? java-class c-args)
            init-mth (if-not (nil? init) (valid-method-sig? java-class init))
            beandef (Bean. *current-context* id java-class mode setters constructor c-args init-mth pre post class-override comments)]
        (add-beandef beandef)
        beandef)
    (catch Exception e# (println (format "Error detected in bean definitions %s: %s" bean-name (.getMessage e#)))
      (print-stack-trace e# 1)
      (root-cause e#)))))

(defn defabean
  "Create an anonymous bean definition. The bean inherits an autogenerated id"
  [jclass & {:keys [s-vals c-args init pre post class-override comments]}]
  (defbean (keyword (gensym "anonymous")) jclass :s-vals s-vals :c-args c-args :init init :pre pre :post post :class-override
    class-override :comments comments))
 
(defn- apply-setter
  "Apply setter to an allocated bean, We recurse through create-bean but the recursion level
   is acceptable. Bean hierarchies are not very deep most of the time and the stack should not blow out.
   A setter value can be:
      - Another embedded bean definition
      - A closure
      - A bean id (keyword) referring to another bean definition in the current context
      - A Java object to pass directly to the setter"
  [instance s]
  (let [{:keys [setter value]} s
        {:keys [mth-arg-class]} (meta setter)]
    (if-not (nil? value)
      (if-let [value (get-value value)]
        (let [equiv-value (to-java-arg (get-value value) mth-arg-class)]
          (try 
            (invoke-method instance setter equiv-value)
            (catch Exception e#
              (throw (Exception. (format "Error in setter: %s\n\tsetter %s\n\tvalue %s\n\tvalue %s" (.getMessage e#) (meta setter)
                                         (class equiv-value) equiv-value))))))))))

(declare bean-info)

(defmacro invoke-wrapper [beandef step & body]
  `(let [id# (:id ~beandef)
         context# (:context ~beandef)]
     (try
       ~@body
     (catch Exception e#
       (println (format "Failed invoking %s step on instance of bean %s in context %s error: %s"
                        ~step id# context# (.getMessage e#)))
       (print-cause-trace e#)))))

(defn- allocate-bean
  "Instantiate an object from an inline bean definition"
  [beandef]
  (try 
    (if-let [singleton (singleton? beandef)]
	    singleton
	    (let [{:keys [constructor c-args setters pre post init id mode] } beandef
            instance (invoke-constructor constructor c-args)]
	      (if-not (nil? pre) (invoke-wrapper beandef "pre-function" (pre instance)))
	      (dorun (map (fn [e] (invoke-wrapper beandef "Setter" (apply-setter instance (val e)))) setters))
	      (if-not (nil? init) (invoke-wrapper beandef "Initialization" (invoke-method instance init)))
	      (if (= mode :singleton) (register-singleton id instance))
	      (if-not (nil? post)       ;; The post fn return value is returned as the object instance
         (invoke-wrapper beandef "post-function" (post instance)) 
          instance)))
    (catch Exception e# (throw (Exception. (.getCause e#))))))

(defn create-bean
  "Return a new instanciation of a bean or the existing singleton if it already exists.
   Either an inline bean definition can be provided or a bean id (keyword).
   The bean definition will be searched either in the current context.
   The current context can be switched using the with-context macro.
   if no context is specified, the bean definition is pulled from the :default context"
  [beandef-or-id]
  (try
    (if (nil? beandef-or-id) nil (get-value beandef-or-id))
    (catch Exception e#
      (print-stack-trace e# 1)
      (root-cause e#))))

(defn- print-bean-info [beandef]
  (let [{:keys [id context java-class mode override-class comments]} beandef]
    (println (format "Bean %s  context: %s  mode: %s  class: %s  class override: %s\nComments: %s"
                     id context mode java-class override-class comments))))
  
(defn bean-info
  "Dump information of a specific bean"
  ([id]
    (if-let [ctx (get-context *current-context*)]
      (if-let [beandef (id (:beandefs ctx))] (print-bean-info beandef)
        (println (format "No such bean %s" id)))))
  ([id ctx-id]
    (if-let [ctx (get-context ctx-id)]
      (if-let [beandef (id (:beandefs ctx))] (print-bean-info beandef)
        (println (format "No such bean %s" id))))))

(defn bean-summary
  "Dump a bean summary of the current context."
  ([]
    (if-let [ctx (get-context *current-context*)]
      (do (println (format "Summary of context %s" *current-context*))
        (dorun (map (fn [[k v]] (println (format "Bean %s  %s\n%s\n" k (:java-class v) (:comments v)))) (:beandefs ctx))))))
  ([ctx-id]
    (if-let [ctx (get-context ctx-id)]
      (do (println (format "Summary of context %s" ctx-id))
        (dorun (map (fn [[k v]] (println (format "Bean %s  %s\n%s\n" k (:java-class v) (:comments v)))) (:beandefs ctx)))))))

(defmacro with-context
  "Using the given context evaluate the given body.
   Bean definitions in the body will be attached to the given context."
  ([id & body]
  `(do (add-context ~id)
   (binding [*current-context* ~id]
     ~@body))))

  
