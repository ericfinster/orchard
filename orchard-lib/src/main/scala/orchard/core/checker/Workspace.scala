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

  def newModule(
    moduleId : String, 
    checkerAddress : CheckerAddress
  ) : Error[Module] =
    for {
      module <- runCommandAtAddress(
        insertModule(moduleId),
        checkerAddress
      )
    } yield module

  def newImport(
    name : String,
    moduleName : String,
    isOpen : Boolean,
    checkerAddress : CheckerAddress
  ) : Error[Import] = 
    for {
      imprt <- runCommandAtAddress(
        insertImport(name, moduleName, isOpen),
        checkerAddress
      )
    } yield imprt

  def newParameter(
    worksheetId : Int, 
    address : CellAddress, 
    identString : String, 
    isThin : Boolean, 
    checkerAddress : CheckerAddress
  ) : Error[Parameter] =
    for {
      worksheet <- getWorksheet(worksheetId)
      targetCell <- worksheet.seek(address)
      _ <- ensure(targetCell.isShell, "Selected cell is not a shell.")
      parameter <- runCommandAtAddress(
        insertParameter(identString, targetCell.neutralNCell, isThin),
        checkerAddress
      )
    } yield {
      targetCell.item = Neutral(Some(parameter.node.variableExpression))
      parameter
    }

  def newDefinition(
    worksheetId : Int,
    address : CellAddress,
    identString : String,
    checkerAddress : CheckerAddress
  ) : Error[Definition] = 
    for {
      worksheet <- getWorksheet(worksheetId)
      targetCell <- worksheet.seek(address)
      _ <- ensure(targetCell.isFillable, "Selected cell is not fillable.")
      definition <- runCommandAtAddress(
        insertDefinition(identString, targetCell.neutralNCell),
        checkerAddress
      )
    } yield {

      targetCell.boundaryFace.item = Neutral(Some(definition.node.fillerExpression.Boundary))
      targetCell.item = Neutral(Some(definition.node.fillerExpression))

      definition

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
      exprNCell <- runCommandAtAddress(
        validatePaste(identifier, targetCell.bindingSkeleton),
        checkerAddress
      )
    } yield {

      // Place all the expressions in their new homes.

      val zippedNCell =  targetCell.skeleton.zip(exprNCell).get

      zippedNCell map {
        case (cell, e) => 
          cell.item match {
            case Neutral(None) => cell.item = Neutral(Some(e))
            case _ => ()
          }
      }

      worksheet

    }
}
