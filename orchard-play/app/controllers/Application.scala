package controllers

import play.api._
import play.api.mvc._

import libs.json._

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._
import orchard.core.checker._

import ErrorM._

object Application extends Controller {

  val checker = new PlayChecker

  


  //============================================================================================
  // OLD VERSION
  //

  val workspace = new Workspace

  def newWorksheet = Action {
    import models.OrchardToPlay._
    val worksheet = workspace.newWorksheet
    Logger.debug("Returning a worksheet with id: " ++ worksheet.hashCode.toString)
    Ok(Json.toJson(success(worksheet.toMarkerComplex)))
  }

  def requestWorksheet = Action(BodyParsers.parse.json) { request =>
    import models.OrchardToPlay._
    val worksheetId = (request.body \ "worksheetId").as[Int]

    val markerComplex = 
      for {
        worksheet <- workspace.getWorksheet(worksheetId)
      } yield worksheet.toMarkerComplex

    Ok(Json.toJson(markerComplex))
  }

  def extrudeWorksheet = Action(BodyParsers.parse.json) { request =>
    import models.OrchardToPlay._

    val worksheetId = (request.body \ "worksheetId").as[Int]
    val selectionDescriptor = (request.body \ "selectionDescriptor").as[SelectionDescriptor]

    val extrudedWorksheet = 
      for {
        worksheet <- workspace.getWorksheet(worksheetId)
        _ <- worksheet.selectFromDescriptor(selectionDescriptor)
        _ <- ensure(worksheet.selectionIsExtrudable, "Selection is not extrudable.")
        _ <- worksheet.emptyExtrusion
      } yield worksheet.toMarkerComplex


    Ok(Json.toJson(extrudedWorksheet))

  }

  def dropWorksheet = Action(BodyParsers.parse.json) { request =>
    import models.OrchardToPlay._

    val worksheetId = (request.body \ "worksheetId").as[Int]
    val selectionDescriptor = (request.body \ "selectionDescriptor").as[SelectionDescriptor]

    val droppedWorksheet = 
      for {
        worksheet <- workspace.getWorksheet(worksheetId)
        _ <- worksheet.selectFromDescriptor(selectionDescriptor)
        _ <- ensure(worksheet.selectionIsDroppable, "Cannot insert a drop here.")
        _ <- worksheet.emptyDrop
      } yield worksheet.toMarkerComplex


    Ok(Json.toJson(droppedWorksheet))

  }

  def paste = Action(BodyParsers.parse.json) { request =>
    import models.OrchardToPlay._

    val worksheetId = (request.body \ "worksheetId").as[Int]
    val cellAddress = (request.body \ "cellAddress").as[CellAddress]
    val checkerAddress = (request.body \ "checkerAddress").as[CheckerAddress]
    val identifier = (request.body \ "identifier").as[String]

    val pastedWorksheet = 
      for {
        worksheet <- workspace.paste(
          worksheetId,
          cellAddress,
          identifier,
          checkerAddress
        )
      } yield worksheet.toMarkerComplex

    Ok(Json.toJson(pastedWorksheet))

  }

  def newModule = Action(BodyParsers.parse.json) { request =>
    import models.OrchardToPlay._

    val moduleId = (request.body \ "moduleId").as[String]
    val checkerAddress = (request.body \ "address").as[CheckerAddress]

    val result : Error[workspace.ModuleEntry] = 
      workspace.newModule(moduleId, checkerAddress)

    Ok(Json.toJson(result))
  }


  def newImport = Action(BodyParsers.parse.json) { request =>
    import models.OrchardToPlay._

    val name = (request.body \ "name").as[String]
    val moduleName = (request.body \ "moduleName").as[String]
    val isOpen = (request.body \ "isOpen").as[Boolean]
    val checkerAddress = (request.body \ "address").as[CheckerAddress]

    val result : Error[workspace.ModuleEntry] = 
      workspace.newImport(name, moduleName, isOpen, checkerAddress)

    println("Finished new import, returning.")

    if (result.isSuccess) {
      println("creation was a success")
    } else {
      println("creation failed")
    }

    Ok(Json.toJson(result))
  }

  def newParameter = Action(BodyParsers.parse.json) { request => 

    import models.OrchardToPlay._

    val worksheetId = (request.body \ "worksheetId").as[Int]
    val cellAddress = (request.body \ "cellAddress").as[CellAddress]
    val identString = (request.body \ "identString").as[String]
    val isThin = (request.body \ "isThin").as[Boolean]
    val checkerAddress = (request.body \ "checkerAddress").as[CheckerAddress]

    val parameterResult : Error[workspace.ModuleEntry] = 
      workspace.newParameter(
        worksheetId,
        cellAddress,
        identString,
        isThin,
        checkerAddress
      )

    Ok(Json.toJson(parameterResult))

  }

  def newDefinition = Action(BodyParsers.parse.json) { request => 

    import models.OrchardToPlay._

    val worksheetId = (request.body \ "worksheetId").as[Int]
    val cellAddress = (request.body \ "cellAddress").as[CellAddress]
    val identString = (request.body \ "identString").as[String]
    val checkerAddress = (request.body \ "checkerAddress").as[CheckerAddress]

    val definitionResult : Error[workspace.ModuleEntry] = 
      workspace.newDefinition(
        worksheetId,
        cellAddress,
        identString,
        checkerAddress
      )

    Ok(Json.toJson(definitionResult))

  }

  def requestEnvironment = Action(BodyParsers.parse.json) { request =>
    import models.OrchardToPlay._

    val checkerAddress = (request.body \ "address").as[CheckerAddress]

    val envResult = workspace.runCommandAtAddress(
      workspace.environmentTree, 
      checkerAddress
    ) 

    Ok(Json.toJson(envResult))

  }

  def requestModule = Action(BodyParsers.parse.json) { request => 
    import models.OrchardToPlay._

    val checkerAddress = (request.body \ "address").as[CheckerAddress]
    val moduleId = (request.body \ "moduleId").as[String]

    val moduleResult : Error[workspace.ModuleEntry] = 
      workspace.runCommandAtAddress(
        workspace.getModule(moduleId),
        checkerAddress
      )

    Ok(Json.toJson(moduleResult))

  }

  //============================================================================================
  // OLD STUFF
  //

  def index = Action {
    workspace.reset
    Ok(views.html.index("This is some content which goes in a panel."))
  }

  def template = Action {
    Ok(views.html.template("Template"))
  }

  def debug = Action(BodyParsers.parse.json) { request =>
    Logger.debug("Received a json post:")
    Logger.debug(Json.prettyPrint(request.body))
    Ok(Json.obj("status" -> "OK", "message" -> "all done"))
  }

}
