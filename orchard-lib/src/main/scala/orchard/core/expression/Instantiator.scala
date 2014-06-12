/**
  * Instantiator.scala - Base trait for definition and module instantiation routines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._

trait Instantiator {

  def shell : Shell
  def defn : ModuleSystem#Definition

  def goals : Seq[Goal] = 
    (defn.parameters ++ defn.localParameters) map 
      ((mp : ModuleSystem#ModuleParameter) => new Goal(mp.variable))

  val referenceExpression = Reference(defn, Immediate)

  var bindings : Map[Int, Expression] = Map.empty

  def wrapVariables(expr : Expression) : Expression = 
    expr match {
      case v : Variable => new Goal(v)
      case _ => expr
    }

  class Goal(v : Variable) extends Variable(v.shell map (wrapVariables(_)), v.index, v.ident, v.isThin) {

    override def styleString = "app"

  }

  def previewExpression : Expression = 
    Substitution(defn.filler.get.Boundary, bindings)

}
