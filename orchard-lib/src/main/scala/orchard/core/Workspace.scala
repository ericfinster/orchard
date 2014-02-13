/**
  * Workspace.scala - A workspace consisting of an environment and a collection of sheets
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.Buffer

trait Workspace extends Environment {

  def name : String

  val sheets = Buffer.empty[ExpressionGallery]
  val environment = Buffer.empty[NCell[Expression]]

  def addToEnvironment(expr : NCell[Expression]) = environment += expr

}
