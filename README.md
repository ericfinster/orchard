# Orchard 

## Description

Orchard is a graphical user interface for manipulating opetopic cells.  It provides
an implementation of a certain definition of opetopic higher category using pictorial
representations of cells.

You can watch some videos about Orchard here:

https://www.youtube.com/playlist?list=PLWAw3zSOqFVKZ-LzAq4hQaKIVj8zjtfCs

I'm afraid there is not much in the way of documentation yet, but feel free to contact
me for more information.  In the meantime, here is a list of keyboard shortcuts which
you can play with:

Ctrl-A  -  Assume variable

Ctrl-E  -  Extrude selection (this will modify the current shape)

Ctrl-D  -  Drop at selection (insert a "nullary" source cell)

Ctrl-C  -  Compose a diagram

Ctrl-I  -  Insert an identity

Ctrl-F  -  Fill a fillable cell

Ctrl-U  -  "Use" the cell currently selected in the environment
           and insert it into the currently selected cell

Ctrl-N  -  Start a new environment, i.e. reset everything

Ctrl-S  -  Save

Crtl-O  -  Open

Left Arrow/Right Arrow  -  Scroll the viewer left and right

You can double click a cell in the environment to open it in a new tab, and you
may do the same for cells in the "expression construction" area.

## Running

You will need to have at least Java 1.7 and JavaFX installed on your machine to run
Orchard.  JavaFX is distributed with the Oracle version of Java available here:

http://www.oracle.com/us/technologies/java/standard-edition/overview/index.html

Note that many Linux distributions come with an open source implementation of the
JVM which does not include JavaFX, so you may have to follow the link above and install
a new one to run Orchard.  You can check if the install is working as follows

```
>> java -version
java version "1.7.0_45"
Java(TM) SE Runtime Environment (build 1.7.0_45-b18)
Java HotSpot(TM) 64-Bit Server VM (build 24.45-b08, mixed mode)
```

Once you have the correct JVM installed, download the binary from the releases section
of the GitHub project.  Place it in any directory you like and type

`java -jar orchard-0.1-SNAPSHOT.jar`

Hopefully this works. :)

## Building

Orchard is written in Scala and uses ScalaFX/JavaFX for graphics.  As above, you will
need to have a version of the JDK which includes JavaFX.  You will also need to install
SBT, the Scala Build Tool.  Versions are available for Windows/Mac and most Linux
distributions will have a binary package available.  See the website at:

http://www.scala-sbt.org/

Once you have Java and SBT running, building the project should be simple.  There is one
extra library dependency, ScalaFX, which provides the graphics bindings in Scala.  Download
the .jar file:

https://code.google.com/p/scalafx/downloads/detail?name=scalafx_2.10-1.0.0-M7.jar

and place it in the directory

orchard/orchard-javafx/lib

(which you should create if it doesn't already exist.)  When this is done, switch to the
main orchard directory, start SBT, and enter the command

`compile`

This should build all the sources.  You should then be able to run the program directly
from SBT by typing 

`run`













