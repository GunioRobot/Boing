(ns boing.resource
  "This module provides a basic interface to resource loading."
  (:require [clojure.string :as s])
  (:use [clojure.stacktrace] [clojure.java.io])
  (:import [java.util.jar JarFile] [java.net URLDecoder]
           [java.lang ClassLoader] [java.io File] [java.util Properties]))

;;(defvar- *font-cache* (Atom {}))

(defn find-url
  ;;"Return the URL of a given resource."
  [respath]
  (try
    (if-let [url (ClassLoader/getSystemResource respath)] url
      (java.net.URL. respath))
    (catch Exception e# (print-cause-trace e#) (throw (Exception. (format "Cannot find URL for resource path %s:%s" respath (.getCause e#)))))))

(defn- access-jar
  "Access a jar file from a url and return a JarFile object to access it"
  [url]
  (let [url-path (.getPath url)
        bang-pos (.indexOf url-path "!")
        jar-path (-> (.getPath url) (.substring 5 bang-pos))]
    (JarFile. (URLDecoder/decode jar-path "UTF-8"))))

(defn enum-resources
  "List resources in the given folder.
   If resources are in a jar file, a class in the jar file must be provided so
   it can be located on the classpath.
   A regex pattern can be provided as a string to filter the resources by their names."
  ([respath & {:keys [from-class pattern]}]
    (try 
      (if (nil? from-class)
        (if-let [url (find-url respath)]
          (cond (= (.getProtocol url) "file") 
                (persistent! conj! (transient []) (-> (File. (.toURI url)) (.list)))
                :else (throw (UnsupportedOperationException.
                               (format "Cannot list files for url %s" url))))
          ())
        (if-let [url (find-url (str (s/replace (.getName from-class) "." "/") ".class"))]
          (cond (= (.getProtocol url) "jar")
                (if-let [jar-file (access-jar url)]
                  (if (nil? pattern)
                    (persistent! (reduce #(if (nil? %1) %1 (conj! %1 %2)) (transient [])
                                         (map #(.getName %) (enumeration-seq (.entries jar-file)))))
                    (persistent! (reduce #(if (nil? %1) %1 (conj! %1 %2)) (transient [])
                                         (map (fn [e] (if (.matches (.getName e) pattern) (.getName e))) (enumeration-seq (.entries jar-file))))))))))
    (catch Exception e# (print-cause-trace e#) (throw e#)))))

(defn load-properties
  "Load a property file as a map."
  ([respath]
    (try 
      (if-let [resource-url (find-url respath)]
        (with-open [stream (input-stream resource-url)]
          (let [properties (doto (Properties.) (.load stream))]
            (persistent! (reduce conj! (transient {})  (map (fn [[k v]] { (keyword k) v}) properties)))))
        {})
      (catch Exception e# (print-cause-trace e#) (throw e#)))))
      
(defn get-input-stream
  ;;"Get an input stream on a resource"
  ([respath & {:keys [from-class]}]
    (try 
      (if-let [resource-url (find-url respath)]
        (cond (nil? from-class)
              (input-stream resource-url)
              :else
              (do respath (.getResourceAsStream from-class respath))))
      (catch Exception e# (print-cause-trace e#) (throw e#)))))
 
      
