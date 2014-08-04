/**
  * PlayChecker.scala - An implementation of the Checker trait as a controller in Play
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package controllers

import orchard.core.util._
import orchard.core.typechecker._

import ErrorM._
import MonadUtils._

class PlayChecker extends Checker {

  var globalModule : Module = Module(new ModuleDefinition("Global", Vector.empty), Vector.empty)

  def runAtLocation[A](addr : Vector[Int], cmd : Command[A]) : Error[A] = 
    for {
      cursor <- ModuleZipper(globalModule, Nil).seek(addr)
      result <- cmd(cursor)
      (newCursor, a) = result
    } yield {
      globalModule = newCursor.zip.asInstanceOf[Module]
      a
    }

}
