# Boing

Boing is a Java dependency injection tool written in Clojure. The main motivation behind
this initiative was a need to get away from Spring beans and all the dependencies that it
carries. Hence the name... if you have better suggestions, they are welcomed.


Usage help will follow, we are still working on the context management.

##Why did we created this ?

We have some Java/Spring legacy code that we want to move to Clojure.
However some Java libraries like ORMs, ... require Java centric
initializations. We want to keep dependency injection for these
initializations. They will still be needed event after we translate our Java code
to Clojure.

At the same time we want to remove Spring and all jar dependencies.

We think that DI the way Boing implements it light weight enough such that
it can be generalized to interface Clojure with Java frameworks simple or complex.

We will see in the following weeks/months how far we can get with this.

##Documentation

Please refer to the Wiki of the project at:
http://gut

##Examples

The examples folder contains presently one comparative example betwen Spring
and Boing. This one is complex. Other examples will follow.

##License

Copyright (C) 2011 Luc Préfontaine, SoftAddicts Inc.

Distributed under the Eclipse Public License, the same as Clojure
uses. 
