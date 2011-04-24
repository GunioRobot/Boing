(ns boing.core.reflector
  "Reflection utility routines"
  (:use
    [clojure.pprint]
    [clojure.contrib.def]
    [clojure.contrib.trace])
  (:import [java.lang.reflect Modifier InvocationTargetException] [boing Util]))

(defvar- +setter-prefix+ "set")

(defvar- *reflection-cache* (atom {}))

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


(defn- get-reflection-cache-item
  "Extract from the cache the required item for the given class.
   Valid items cached are: :setters"
  [java-class item]
  (let [hashcode (java.lang.System/identityHashCode java-class)]
    (if-let [item-cached (item (get @*reflection-cache* hashcode))]
      item-cached
      nil)))

(defn- update-reflection-cache
  "Updates the reflection cache with the provided class.
   Returns the cached items for the given class.
   Valid items cached are: :setters"
  [java-class items-map]
  (let [hashcode (java.lang.System/identityHashCode java-class)]
    (if-let [items-cached (get @*reflection-cache* hashcode)]
      (get (swap! *reflection-cache* #(merge %1 %2) {hashcode (merge items-cached items-map)}) hashcode)
      (get (swap! *reflection-cache* #(merge %1 %2) {hashcode items-map}) hashcode))))

(defn find-all-setters
  "Return all the setter methods for this class and its super classes
   as a hash indexed by the property name, the static setters are not retained
   If we already computed the setter map for this class, it's in the cache so we can return it immediately"
  [java-class]
  (if-let [setter-map (get-reflection-cache-item java-class :setters)]
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

