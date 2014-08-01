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
import orchard.core.complex._

import ErrorM._
import ErrorMonad._

class Workspace extends Checker {

  var rootModule : Error[Module] = 
    Right(Module(new CheckerModuleNode("Prelude"), Vector.empty))

  def reset : Unit = {
    worksheetMap.clear
    rootModule = Right(Module(new CheckerModuleNode("Prelude"), Vector.empty))
  }

  def rootZipper : Error[ModuleZipper] = 
    for {
      rootM <- rootModule
    } yield ModuleZipper(rootM, Nil)

  val worksheetMap : HashMap[Int, Worksheet] = HashMap.empty

  def newWorksheet : Worksheet = {
    val worksheet = Worksheet()
    worksheetMap(worksheet.hashCode) = worksheet
    worksheet
  }

  def runCommand[A](cmd : CheckerM[A]) : Error[A] = 
    runCommandAtAddress(cmd, CheckerAddress(Vector.empty, 0))

  def runCommandAtAddress[A](cmd : CheckerM[A], addr : CheckerAddress) : Error[A] = 
    for {
      root <- rootModule
      zipper <- (ModuleZipper(root, Nil)).seek(addr.moduleAddress)
      res <- (cmd run (zipper,  addr.cursorOffset))
    } yield {
      val newRoot = res._1._1.zip.asInstanceOf[Module]
      println(newRoot.toString)
      rootModule = Right(newRoot)
      res._2
    }


  def getWorksheet(worksheetId : Int) : Error[Worksheet] =
    if (worksheetMap.isDefinedAt(worksheetId))
      success(worksheetMap(worksheetId))
    else
      fail("Requested worksheet not found.")

  def newParameter(
    worksheetId : Int, 
    address : CellAddress, 
    identString : String, 
    isThin : Boolean, 
    checkerAddress : CheckerAddress
  ) : Error[String] =
    for {
      worksheet <- getWorksheet(worksheetId)
      targetCell <- worksheet.seek(address)
      _ <- ensure(targetCell.isShell, "Selected cell is not a shell.")
      shell = new Shell(targetCell.neutralNCell)
      parameterNode <- runCommandAtAddress(
        insertParameter(identString, shell, isThin),
        checkerAddress
      )
    } yield {
      targetCell.item = Neutral(Some(parameterNode.variableExpression))
      parameterNode.name
    }

  def newDefinition(
    worksheetId : Int,
    address : CellAddress,
    identString : String,
    checkerAddress : CheckerAddress
  ) : Error[String] = 
    for {
      worksheet <- getWorksheet(worksheetId)
      targetCell <- worksheet.seek(address)
      _ <- ensure(targetCell.isFillable, "Selected cell is not fillable.")
      nook = new Nook (targetCell.neutralNCell, targetCell.isThinBoundary)
      definitionNode <- runCommandAtAddress(
        insertDefinition(identString, nook),
        checkerAddress
      )
    } yield {

      targetCell.boundaryFace.item = Neutral(Some(definitionNode.fillerExpression.Boundary))
      targetCell.item = Neutral(Some(definitionNode.fillerExpression))

      // Put the expressions in the worksheet
      definitionNode.name

    }

  def paste(
    worksheetId : Int,
    address : CellAddress,
    identifier : String,
    checkerAddress : CheckerAddress
  ) : Error[Worksheet] = 
    for {
      worksheet <- getWorksheet(worksheetId)
      targetCell <- worksheet.seek(address)
      _ <- ensure(targetCell.isEmpty, "Selected cell is not empty.")
    } yield {
      worksheet
    }
}
