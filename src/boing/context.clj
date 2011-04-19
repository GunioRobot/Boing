(ns boing.context
  (:use [boing.core.types]
    [clojure.contrib.def]
    [clojure.contrib.trace]))

(defvar- *contexts* (atom {}))
(defvar *current-context* nil)  ;; This can be rebound on the fly

(defn get-current-context []
  *current-context*)

(defn get-context
  "Returns the given context by its id, nil if none exists"
  [id]
  (if-let [ctx (id @*contexts*)] ctx))

(defn add-beandefs
  "Add bean definitions to current context, throw an error if the configuration
   is not already registered"
  ([id beandefs]
    (swap! *current-context* #(merge %1 %2)
      {id (merge (id @*contexts*) beandefs)})
    (throw (Exception. (format "Configuration %s does not exists"))))
  ([ctx-id beandefs
    (if-let [ctx (get-context ctx-id)]
      ()
      (throw (Exception. (format "No such context %s" ctx-id))))))

(defn new-context
  "Registers a new bean context and returns it"
  [id beans]
  (swap! *contexts* #(merge %1 %2) {id {}})
  (add-beandefs id beans))

(defn merge-context
  "Merge two contexts, beans with the same ids will be replaced by
   the instances in the 'from' context"
  [from to]
  (let [merger (merge (to @*contexts*) (from @*contexts*))]
    (swap! *contexts* #(merge %1 %2) { to merger })))

(defn get-bean
  "Fetch a bean definition from the current or the given context and instanciate it."
  ([id]
    (if-let [beandef (id (get-current-context))]
      (create-bean beandef)
      (throw (Error. (format "Cannot find bean %s in default context") id))))
  ([ctx-id id]
    (if-let [beandef (id (get-context ctx-id))]
      (create-bean beandef)
      (throw (Error. (format "Cannot find bean %s in context %s") id ctx-id)))))
