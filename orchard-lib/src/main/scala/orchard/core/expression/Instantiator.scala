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
  def wksp : Workspace

  def activeGoal : Option[Goal]
  def activeExpression : Option[Expression]

  def goals : Seq[Goal] = 
    (defn.parameters ++ defn.localParameters) map 
      ((mp : ModuleSystem#ModuleParameter) => new Goal(mp.variable))

  var bindings : Map[Int, Expression] = Map.empty

  def wrapVariables(expr : Expression) : Expression = 
    expr match {
      case v : Variable => new Goal(v)
      case _ => expr
    }

  class Goal(v : Variable) extends Variable(v.shell map (wrapVariables(_)), v.index, v.ident, v.isThin) {

    override def name : String = 
      if (bindings.isDefinedAt(v.index)) {
        v.name ++ " -> " ++ bindings(v.index).name
      } else {
        v.name
      }

    override def styleString = 
      if (bindings.isDefinedAt(v.index)) {
        bindings(v.index).styleString
      } else "app"

  }

  def refreshPreview : Unit

  def previewExpression : Expression = 
    Substitution(defn.filler.get.Boundary, bindings)

  def bind : Unit = 
    for {
      goal <- activeGoal
      expr <- activeExpression
    } {

      wksp.editor.consoleMessage("Attempting to bind " ++ expr.toString ++ " to " ++ goal.toString)

      goal.ncell.zip(expr.ncell) match {
        case None => wksp.editor.consoleError("Shape incompatibility")
        case Some(zippedTree) => {

          var newBindings : Map[Int, Expression] = Map.empty

          try {
            zippedTree map {
              case (gExpr, bExpr) => {
                gExpr match {
                  case g : Goal => {
                    val idx = g.index

                    if (bindings.isDefinedAt(idx)) {
                      if (bindings(idx).normalize != bExpr.normalize) {
                        wksp.editor.consoleError("Match error: " ++ gExpr.toString ++ " is already bound to " ++
                          bindings(idx).toString ++ " which is not convertible to " ++ bExpr.toString)
                        throw new IllegalStateException
                      }
                    } else if (newBindings.isDefinedAt(idx)) {
                      if (newBindings(idx).normalize != bExpr.normalize) {
                        wksp.editor.consoleError("Match error: " ++ gExpr.toString ++ " is already bound to " ++
                          bindings(idx).toString ++ " which is not convertible to " ++ bExpr.toString)
                        throw new IllegalStateException
                      }
                    } else {
                      newBindings = (newBindings + (g.index -> bExpr))
                    }
                  }
                  case _ =>
                    if (gExpr.normalize != bExpr.normalize) {
                      wksp.editor.consoleError("Match error: " ++ gExpr.toString ++ " is not convertible to " ++ bExpr.toString)
                      throw new IllegalStateException
                    }
                  
                }
              }
            }
          } catch {
            case e : Exception => wksp.editor.consoleError("Binding failed.")
          }

          // This should mean the guys are compatible ...
          bindings = bindings ++ newBindings

          refreshPreview
        }
      }
    }

}
