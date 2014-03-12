/**
  * Definition.scala - A class encapsulating a definition
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import Environment._
import Identifier._

class Definition(
  val name : String,
  val stabilityLevel : Option[Int],
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int], 
  val environment : Seq[NCell[Expression]],
  val output : Expression
) {

  def bindingsFromEnvironment(env : Seq[Expression]) : Map[String, Expression] = {
    HashMap.empty ++ ((environment.vars map (_.value.id)) zip env)
  }

  def outputExpr : NCell[Expression] = environment.lookup(output.id).get


  override def toString = name

}
