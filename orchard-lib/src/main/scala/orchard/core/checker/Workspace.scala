/**
  * Workspace.scala - A workspace with a type checker and worksheets
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import scala.collection.mutable.HashMap

import orchard.core.util._
import orchard.core.cell._

import ErrorM._
import ErrorMonad._

class Workspace extends Checker {

  var rootModule : Error[Module] = 
    Right(Module(new CheckerModuleNode("Prelude"), Vector.empty))

  val worksheetMap : HashMap[Int, Worksheet] = HashMap.empty

  def newWorksheet : Worksheet = {
    val worksheet = Worksheet()
    worksheetMap(worksheet.hashCode) = worksheet
    worksheet
  }

  def runCommand[A](cmd : CheckerM[A]) : Error[A] = 
    for {
      root <- rootModule
      res <- (cmd run (ModuleZipper(root, Nil)))
    } yield {
      rootModule = Right(res._1.zip.asInstanceOf[Module])
      res._2
    }

  def runCommandAtAddress[A](cmd : CheckerM[A], addr : CheckerAddress) : Error[A] = 
    for {
      root <- rootModule
      ptr <- (ModuleZipper(root, Nil)).seek(addr.moduleAddress)
      res <- (cmd run ptr)
    } yield {
      rootModule = Right(res._1.zip.asInstanceOf[Module])
      res._2
    }

  def newParameter(workspaceId : Int, address : CellAddress, identString : String) : Error[Unit] = {
    if (worksheetMap.isDefinedAt(workspaceId)) {

      // Now what?  Well, aside from the string parsing and whatnot, don't we need to know some
      // kind of location in the module to see what exactly is in scope?

      // Yes, we do.  A new case class?  CheckerAddress?  It should consist of a Vector[Int] specifying
      // the module which we are in and an Int specifying the cursor offset.  With this information,
      // we can ask the type checker for the currently valid environment and then check to see if there
      // is any kind of identifier conflict.

      // Right.  We're going to need this to be up and running before we can do any interesting semantic
      // manipulation.  So I think that is the next task.

      success(())
    } else {
      fail("Requested worksheet not found.")
    }
  }

}
