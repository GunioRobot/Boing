(ns boing.context
  (:use
    [clojure.contrib.def]
    [clojure.contrib.trace]))

(defrecord Context [id beandefs])

(defvar- *contexts* (atom {:default (Context. :default {})}))

(defvar *current-context* :default)  ;; This can be rebound on the fly

(defn get-context
  "Returns the given context by its id, nil if none exists"
  [id]
  (if-let [ctx (id @*contexts*)] ctx))

(defn add-beandef
  "Add a bean definition to the current context."
  ([beandef]
    (if-let [ctx (*current-context* @*contexts*)]
      (swap! *contexts* #(merge %1 %2)
        {*current-context* (Context. *current-context* (assoc (:beandefs ctx) (:id beandef) beandef))})
      (throw (Exception. (format "No such context %s" *current-context*))))
    ))

(defn get-bean-ids
  "Return the ids of the bean definitions in the current context."
  []
  (if-let [ctx (*current-context* @*contexts*)]
    (apply vector (sort (keys (:beandefs ctx))))
    (throw (Exception. (format "No such context %s" *current-context*)))))

(defn add-context
  "Registers a new bean context if it does not exists and returns it"
  [id]
  (if-let [ctx ((keyword id) *contexts*)]
    (swap! *contexts* #(merge %1 %2) { (keyword id) {}})
    (swap! *contexts* #(merge %1 %2) { (keyword id) (Context. id {})})))

(defn merge-context
  "Merge two contexts, beans with the same ids will be replaced by
   the instances in the 'from' context"
  [from to]
  (let [merger (merge ((keyword to) @*contexts*) ((keyword from) @*contexts*))]
    (swap! *contexts* #(merge %1 %2) { to merger })))

(defn find-beandef
  ([id]
    (if-let [ctx (*current-context* @*contexts*)]
      ((keyword id) (:beandefs ctx))
      (throw (Exception. (format "No such bean %s in context %s" id *current-context*)))))
  ([ctx-id id]
    (if-let [ctx ((keyword ctx-id) @*contexts*)]
      ((keyword id) ctx)
      (throw (Exception. (format "No such bean %s in context %s" id ctx-id))))))


