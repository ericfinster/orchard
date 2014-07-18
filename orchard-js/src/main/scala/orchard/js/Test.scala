/**
  * Test.scala - Testing the js compiler
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.scalajs.js.JSApp

import scala.collection.mutable.ListBuffer

import orchard.core.util.RoseTree

object Test extends JSApp {
  def main(): Unit = {

    val test : ListBuffer[Int] = ListBuffer(1, 2, 3, 4)

    for {
      i <- test
    } {
      println("Vector contains: " ++ i.toString)
    }

    println("Hello world!")
  }
}
