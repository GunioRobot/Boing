(defproject Boing "1.0"
  :min-lein-version "1.5.1"
  :disable-implicit-clean false
  :java-source-path "src"
  :java-fork true
  :javac-target "1.5"
  :warn-on-reflection false
  :target-dir "target/"
  :jar-name "boing.jar"
  :manifest {"Built-By" "build-manager"
             "Specification-Title" "Boing library"
             "Specification-Version" "1.0"
             "Specification-Vendor" "SoftAddicts Inc."
             "Implementation-Title" "Boing library"
             "Implementation-Version" 	"1.4.0"
             "Implementation-Vendor" "SoftAddicts Inc."
             }
  :omit-source false
  :aot :all
  :dependencies [
  	[org.clojure/clojure "1.2.0"]
  	[org.clojure/clojure-contrib "1.2.0"]]
)
