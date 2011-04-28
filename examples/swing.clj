(ns swing
  "This example is to show how boing can be used to create Swing GUIs"
  (:use [boing.bean] [boing.context] [boing.resource] [clojure.contrib.def]))

(def *label* nil)
(def *action-listener* nil)

(defn actl [f]
  (proxy [ActionListener] []
    (actionPerformed [event] (apply f event args))))

(defbean :jbutton javax.swing.JButton
  :c-args [ (override :label) ]
  :s-vals {:actionListener (override :action-listener)})

(defn new-btn [label actl]
  (create-bean :jbutton {: