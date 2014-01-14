# Orchard 

## Description

Orchard is a graphical user interface for manipulating opetopic cells.  The ultimate
aim is to have a kind of graphical proof assitant for higher category theory.

## Running

You will need to have at least Java 1.7 and JavaFX installed on your machine to run
Orchard.  JavaFX is distributed with the Oracle version of Java available here:

http://www.oracle.com/us/technologies/java/standard-edition/overview/index.html

Note that many Linux distributions come with an open source implementation of the
JVM which does not include JavaFX, so you may have to follow the link above and install
a new one to run Orchard.

## Building

Orchard is written in Scala and uses ScalaFX/JavaFX for graphics.  As above, you will
need to have a version of the JDK which includes JavaFX.  You will also need to install
SBT, the scala build tool.  Versions are available for Window/Mac and most Linux
distributions will have a binary package available.  See the website at:

http://www.scala-sbt.org/

Once you have Java and SBT running, building the project should be simple.  There is one
extra library dependency, ScalaFX, which provide the graphics bindings in Scala.  Download
the .jar file:

https://code.google.com/p/scalafx/downloads/detail?name=scalafx_2.10-1.0.0-M7.jar

and place it in the directory

orchard/orchard-javafx/lib

(which you should create if it doesn't already exist.)  When this is done, switch to the
main orchard directory and do start SBT and enter the command

`compile`

This should build all the sources.  You should then be able to run the program directly
from SBT by typing 

`run`













