package controllers

import play.api._
import play.api.mvc._

import libs.json._

import orchard.core.cell._
import models.PlayComplex

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Fred."))
  }

  def getComplex = Action { 
    val complex = new PlayComplex(Example1.w map (_.toString))
    val json = Json.toJson(complex)
    Ok(json)
  }

}
