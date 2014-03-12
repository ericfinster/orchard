/**
  * Expression.scala - Simple opetopic expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Buffer

import Identifier._

sealed trait Expression { 

  def ident : Identifier 
  def isThin : Boolean

  def id = ident.toString

  override def toString = id
}

case class Variable(val ident : Identifier, val isThin : Boolean) extends Expression { 

  override def toString = id
}

case class Application(val defn : Definition, val args : Seq[NCell[Expression]], val shell : NCell[Option[Expression]]) extends Expression {

  val bindings = defn.bindingsFromEnvironment(args map (_.value))
  val ident = defn.output.ident.translateWithBindings(bindings)

  def isThin : Boolean = defn.output.isThin

}

case class FillerFace(val ident : Identifier, val filler : String, val isThin : Boolean) extends Expression
case class Filler(val ident : Identifier) extends Expression { def isThin : Boolean = true }
case class UnicityFiller(val ident : Identifier) extends Expression { def isThin : Boolean = true }

case class Projection(val addr : Seq[Int]) extends Expression {

  // Okay, the problem here is that expressions are not really at all connected
  // with the environment.  So we can't really use the environment in the definition
  // to lookup the corrent values in the environment ...

  // One solution is to make the expressions live in the workspace.  The editor, you'll notice
  // is already not to dependent on the expressions, not many things are left.  But this will
  // kind of mess up the xml writing.

  // A simpler solution is probably to just override the identifier definition when they are
  // created, although you will probably want to change this setup later

  def ident : Identifier = ???
  def isThin : Boolean = ???

}
