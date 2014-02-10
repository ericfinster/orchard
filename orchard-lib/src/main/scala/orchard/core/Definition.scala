/**
  * Definition.scala - A class encapsulating a definition
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Buffer

class Definition(val id : String, val environment : Seq[NCell[Expression]]) {

  def exprIsVariable(expr : Expression) : Boolean = 
    expr match {
      case Variable(_, _) => true
      case _ => false
    }

  def freeVariables : Seq[NCell[Expression]] = {
    environment filter (exprCell => exprIsVariable(exprCell.value))
  }

  def derivedCells : Seq[NCell[Expression]] = {
    environment filter (exprCell => ! exprIsVariable(exprCell.value))
  }

  override def toString = id
}

object Definition {

  def toXML(defn : Definition) : xml.Node = ???

  def fromXML(node : xml.Node) : Definition = ???

}
