package controllers

import play.api._
import play.api.mvc._

import libs.json._

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._
import orchard.core.checker._

import ErrorM._
import models.PlayComplex

object Application extends Controller {

  val workspace = new Workspace

  def newWorksheet = Action {
    import models.OrchardToPlay._
    val worksheet = workspace.newWorksheet
    Logger.debug("Returning a worksheet with id: " ++ worksheet.hashCode.toString)
    Ok(Json.obj(
      "status" -> "OK",
      "message" -> Json.toJson(worksheet.toMarkerComplex)
    ))
  }

  def requestEnvironment = Action(BodyParsers.parse.json) { request =>
    import models.OrchardToPlay._

    val checkerAddress = (request.body \ "address").as[CheckerAddress]

    workspace.runCommandAtAddress(workspace.environmentTree, checkerAddress) match {
      case Left(msg) => Ok(Json.obj("status" -> "KO", "message" -> msg))
      case Right(envTree) => Ok(Json.obj("status" -> "OK", "message" -> Json.toJson(envTree)))
    }

  }

  def requestWorksheet = Action(BodyParsers.parse.json) { request =>
    val worksheetId = (request.body \ "worksheetId").as[Int]

    if (! workspace.worksheetMap.isDefinedAt(worksheetId)) {
      BadRequest(Json.obj("status" -> "KO", "message" -> "Requested worksheet not found."))
    } else {
        import models.OrchardToPlay._
        val worksheet = workspace.worksheetMap(worksheetId)
        Ok(Json.obj(
          "status" -> "OK", 
          "message" -> Json.toJson(worksheet.toMarkerComplex)
        ))
    }
  }

  def extrudeWorksheet = Action(BodyParsers.parse.json) { request =>

    val worksheetId = (request.body \ "worksheetId").as[Int]

    if (! workspace.worksheetMap.isDefinedAt(worksheetId)) {
      Ok(Json.obj("status" -> "KO", "message" -> "Requested worksheet not found."))
    } else {
      import models.OrchardToPlay._

      Logger.debug("Extruding on worksheet: " ++ worksheetId.toString)

      val worksheet = workspace.worksheetMap(worksheetId)
      val descriptorResult = (request.body \ "selectionDescriptor").validate[SelectionDescriptor]

      descriptorResult.fold(
        errors => {
          BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toFlatJson(errors)))
        },
        descriptor => {
          val extrusionResult =
            for {
              _ <- worksheet.selectFromDescriptor(descriptor)
              _ <- ensure(worksheet.selectionIsExtrudable, "Selection is not extrudable.")
              _ <- worksheet.emptyExtrusion
            } yield ()

          extrusionResult match {
            case Right(()) => {
              Ok(Json.obj("status" -> "OK", "message" -> Json.toJson(worksheet.toMarkerComplex)))
            }
            case Left(msg) => {
              Ok(Json.obj("status" -> "KO", "message" -> msg))
            }
          }
        }
      )
    }
  }

  def newModule = Action(BodyParsers.parse.json) { request =>
    import models.OrchardToPlay._

    val moduleId = (request.body \ "moduleId").as[String]
    val checkerAddress = (request.body \ "address").as[CheckerAddress]

    Logger.debug("Requesting module: " ++ moduleId)

    val insertionCommand = workspace.insertModule(moduleId, checkerAddress.cursorOffset)

    workspace.runCommandAtAddress(insertionCommand, checkerAddress) match {
      case Left(msg) => Ok(Json.obj("status" -> "KO", "message" -> msg))
      case Right(moduleNode) => Ok(Json.obj("status" -> "OK").toString)
    }
  }

  def newParameter = Action(BodyParsers.parse.json) { request => 

    val worksheetId = (request.body \ "worksheetId").as[Int]

    if (! workspace.worksheetMap.isDefinedAt(worksheetId)) {
      Ok(Json.obj("status" -> "KO", "message" -> "Requested worksheet not found."))
    } else {
      import models.OrchardToPlay._

      val worksheet = workspace.worksheetMap(worksheetId)

      val cellAddress = (request.body \ "cellAddress").as[CellAddress]
      val identString = (request.body \ "identString").as[String]

      // Now what? We need to pass this off to the workspace which should check that the cell
      // can be filled with a parameter and parse the identifier ...

      // Great, now here's the problem: should we check that the body of the pointed to cell
      // is valid in the current environment?  Yes, I think so.  There can be some kind of
      // more elaborate worksheet management later.  For now we will just do as mentioned:
      // look at the identifiers on the shell and make sure they are in fact visible in
      // the scope provided.  If not, we will reject the request at this point by saying
      // so.

      // So, for the next step, we're going to have to resurrect the identifier parsing and 
      // start getting serious about how these guys are stored.  Ok, ok.  We'll get there.

      Ok(Json.obj("status" -> "OK"))
    }

  }

  //============================================================================================
  // OLD STUFF
  //

  def index = Action {
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
