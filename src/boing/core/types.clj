(ns boing.core.types)

(defrecord Context [id beans])
(defrecord Bean [context id java-class mode setters constructor
                 constructor-args factory init post])




