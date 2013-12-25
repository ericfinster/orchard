/**
  * Expression.scala - Simple opetopic expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

sealed trait Expression { def id : String ; def isThin : Boolean }

case class Variable(val id : String, val isThin : Boolean) extends Expression { 
  override def toString = id 
}

// case class Filler(val id : String, val nook : NCell[Option[Expression]]) extends Expression { 
case class Filler(val id : String, val nook : (RoseTree[Option[String], Option[String]], Option[String])) extends Expression {
  def isThin = true 
  override def toString = id 
}

// case class FillerTarget(val id : String, val nook : NCell[Option[Expression]], val isThin : Boolean) extends Expression {
case class FillerTarget(val id : String, val nook : (RoseTree[Option[String], Option[String]], Option[String]), val isThin : Boolean) extends Expression { 
  override def toString = id
}

