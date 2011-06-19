### 1.0 (2011-04-27)

* First version

### 1.1 (2011-05-14)

* Fixed a bug with singleton using factories, was crashing after first instantiation (class cast exception)
* Optimized the code with transients
* Added the possibility to instantiate singletons at bean definition time instead of lazy initialization
* Added the ability to alias property names across all bean definitions
* Allow a caller to add argument auto-promotions
* Added resource input streams

### 1.2 (2011-06-17)

* Added a Java API to load bean definitions written in Clojure and to instantiate beans
* Fixed overrides when using constructor values. Overrides were not
  executed.



