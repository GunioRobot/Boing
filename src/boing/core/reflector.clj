(ns boing.core.reflector
  "Reflection utility routines"
  (:use
    [clojure.pprint]
    [clojure.contrib.def]
    [clojure.contrib.trace])
  (:import [java.lang.reflect Modifier InvocationTargetException] [boing Util]))

(defvar- +setter-prefix+ "set")
(defvar- +array-fn+
  {
   (java.lang.Class/forName "[Ljava.lang.String;") (fn [s] (into-array java.lang.String s))
   (java.lang.Class/forName "[I") (fn [s] (int-array s))
   (java.lang.Class/forName "[J") (fn [s] (long-array s))
   (java.lang.Class/forName "[S") (fn [s] (short-array s))
   (java.lang.Class/forName "[B") (fn [s] (byte-array s))
   (java.lang.Class/forName "[C") (fn [s] (char-array s))   
   (java.lang.Class/forName "[Z") (fn [s] (boolean-array s)) 
   (java.lang.Class/forName "[F") (fn [s] (float-array s))
   (java.lang.Class/forName "[D") (fn [s] (double-array s))
   })


(defvar- *reflection-cache* (atom {}))

(defn- get-reflection-cache
  "Extract from the cache the required item for the given class.
   Valid items cached are: :setters and :interfaces"
  [java-class item]
  (let [hashcode (java.lang.System/identityHashCode java-class)]
    (if-let [item-cached (item (get @*reflection-cache* hashcode))]
      item-cached
      nil)))

(defn- update-reflection-cache
  "Updates the reflection cache with the provided class.
   Returns the cached items for the given class.
   Valid items cached are: :setters :interfaces"
  [java-class items-map]
  (let [hashcode (java.lang.System/identityHashCode java-class)]
    (if-let [items-cached (get @*reflection-cache* hashcode)]
      (get (swap! *reflection-cache* #(merge %1 %2) {hashcode (merge items-cached items-map)}) hashcode)
      (get (swap! *reflection-cache* #(merge %1 %2) {hashcode items-map}) hashcode))))

(defn- list-if?
  "Returns true if the given class implements the List interface.
   Class interfaces are cached for future use."
  [cl]
  (if-let [ifs (get-reflection-cache cl :interfaces)]
    (get ifs cl)
    (update-reflection-cache cl (map (fn [if] {(identity if) true}) (:interfaces (bean cl))))))

(defn- map-if?
  "Returns true if a class implements the java.util.Map interface.
   Class interfaces are cached for future use."
  [cl]
  (if-let [ifs (get-reflection-cache cl :interfaces)]
    (get ifs cl)
    (update-reflection-cache cl (map (fn [if] {(identity if) true}) (:interfaces (bean cl))))))

(defprotocol BoingReflector
  "This protocol helps the reflector to adapt Clojure values to java setter argument signatures."
  (to-java-arg [x target-class]))

(extend-type clojure.lang.LazySeq
  BoingReflector
  (to-java-arg
    [this target-class]
    (let [fn-array (get +array-fn+ target-class)]
      (cond
        fn-array (fn-array this)
        (list-if? target-class) (doall this)
        (and (map? this) (map-if? target-class)) (doall this)       
        :else (object-array (doall this))))))

(extend-type clojure.lang.PersistentVector
  BoingReflector
  (to-java-arg
    [this target-class]
    (let [fn-array (get +array-fn+ target-class)]
      (cond
        (= target-class java.util.Vector) (java.util.Vector. this)
        fn-array (fn-array this)
        :else this))))

(extend-type clojure.lang.PersistentList
  BoingReflector
  (to-java-arg
    [this target-class]
    (let [fn-array (get +array-fn+ target-class)]
      (cond
        fn-array (fn-array this)
        :else this))))

(defn- map-to-properties [m]
  (let [java-props (java.util.Properties.)]
    (dorun (map (fn [e] (.put java-props (name (key e)) (val e))) m))
    java-props))

(extend-type clojure.lang.PersistentHashMap
  BoingReflector
  (to-java-arg
    [this target-class]
    (cond
      (= target-class java.util.Properties) (map-to-properties this)
      :else this)))

(extend-type clojure.lang.PersistentArrayMap
  BoingReflector
  (to-java-arg
    [this target-class]
    (cond
      (= target-class java.util.Properties) (map-to-properties this)
      :else this)))

(extend-type java.lang.Object
  BoingReflector
  (to-java-arg [this target-class] this))

(defn setter-to-prop
  "Derive the property name from the setter name"
  [setter]
  (let [prop-name (.replaceFirst setter +setter-prefix+ "")]
  (keyword (str (.toLowerCase (str (first prop-name ))) (apply str (rest prop-name))))))

(defn find-class-setters
  "Return all the setter methods for the given class as a hash indexed by the property name,
   the static setters are not retained. Setter methods are wrapped in a function.
   The setter's argument class is added to the function object for future validation.
   We assume that there is only one setter per property. This may be too simplistic however.
   Next release will index by property name and signature."
  [java-class]
  (into {}
    (doall (remove nil? (map (fn [mth]
                               (let [properties (bean mth)
                                     modifiers (:modifiers properties)
                                     mth-name (:name properties)
                                     static? (pos? (bit-and modifiers Modifier/STATIC))]
                                 (cond
                                   static? {}
                                   (.startsWith mth-name +setter-prefix+)
                                   { (setter-to-prop mth-name)
                                    (with-meta (fn [o p] (.invoke mth o (to-array [p])))
                                      {:mth-name (.getName mth) :mth-arg-class (aget (.getParameterTypes mth) 0)})}
                                   :else {})))
                          (:declaredMethods (bean java-class)))))))

(defn find-all-setters
  "Return all the setter methods for this class and its super classes
   as a hash indexed by the property name, the static setters are not retained
   If we already computed the setter map for this class, it's in the cache so we can return it immediately"
  [java-class]
  (if-let [setter-map (get-reflection-cache java-class :setters)]
    setter-map
    (loop [setter-map (transient {}) curr-class java-class]
      (if (nil? curr-class)
        (:setters (update-reflection-cache java-class {:setters (persistent! setter-map)}))
        (recur (conj! setter-map (find-class-setters curr-class)) (:genericSuperclass (bean curr-class)))))))

(defn valid-constructor?
  "Check if a constructor exists with the signature of the given args,
   if no args are specified, return the noargs constructor"
  [java-class args]
  (let [signature (into [] (doall (map #(Util/getPrimitiveClass (class %)) args)))]
    (try 
      (.getConstructor java-class (into-array java.lang.Class signature))
    (catch NoSuchMethodException e#
      (cond
        (instance? e# Exception) (throw (Exception.  (format "No constructor found in class %s for arguments %s" java-class signature)))
        (instance? e# Error) (throw (Error. (format "No constructor found in class %s for arguments %s" java-class signature))))))))      


(defn valid-method-sig?
  "Check if a method exists with a valid signature mathing the given args.
   Returns the method if found, otherwise throw an error/exception."
  ([java-class method args]
  (let [signature (into [] (doall (map #(Util/getPrimitiveClass (class %)) args)))]
    (try 
      (.getMethod java-class method (into-array java.lang.Class signature))
    (catch NoSuchMethodException e#
      (cond
        (instance? e# Exception) (throw (Exception.  (format "No method %s found in class %s for arguments %s" method java-class signature)))
        (instance? e# Error) (throw (Error. (format "No method %s found in class %s for arguments %s" method java-class signature))))))))

  ([java-class method]
    (try 
      (.getMethod java-class method (into-array java.lang.Class []))
      (catch NoSuchMethodException e#
        (cond
          (instance? e# Exception) (throw (Exception.  (format "No method found in class %s" java-class)))
          (instance? e# Error) (throw (Error. (format "No method found in class %s" java-class))))))))

(defn invoke-constructor
  "Invoke constructor with the given args."
  ([constructor args]
    (try
      (.newInstance constructor (to-array args))
      (catch InvocationTargetException e#
        (cond
          (instance? e# Exception) (throw (Exception. (.getCause e#)))
          (instance? e# Error) (throw (Error. (.getCause e#)))))))
  ([constructor]
  (try
    (.newInstance constructor (to-array []))
    (catch InvocationTargetException e#
      (cond
        (instance? e# Exception) (throw (Exception. (.getCause e#)))
        (instance? e# Error) (throw (Error. (.getCause e#))))))))    

(defn invoke-method
  "Invoke the given setter with its values"
  ([instance method values]
    (try
      (.invoke method instance values)
      (catch InvocationTargetException e#
        (cond
          (instance? e# Exception) (throw (Exception. (.getCause e#)))
          (instance? e# Error) (throw (Error. (.getCause e#)))))))

  ([instance method]
    (try
      (.invoke method instance (to-array []))
      (catch InvocationTargetException e#
        (cond
          (instance? e# Exception) (throw (Exception. (.getCause e#)))
          (instance? e# Error) (throw (Error. (.getCause e#))))))))


