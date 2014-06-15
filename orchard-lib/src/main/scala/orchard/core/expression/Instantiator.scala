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

  def lift : ModuleSystem#Lift
  def wksp : Workspace

  def activeGoal : Option[Goal]
  def activeExpression : Option[Expression]

  val goals : Seq[Goal] = 
    lift.parameters map ((p : ModuleSystem#Parameter) => new Goal(p.variable))

  var bindings : Map[Int, Expression] = Map.empty

  def isComplete : Boolean = 
    goals forall (_.isBound)

  def wrapVariables(expr : Expression) : Expression = 
    expr match {
      case v : Variable => new Goal(v)
      case _ => expr
    }

  class Goal(v : Variable) extends Variable(v.shell map (wrapVariables(_)), v.index, v.ident, v.isThin) {

    def isBound : Boolean = 
      bindings.isDefinedAt(v.index)

    override def name : String = 
      if (isBound) {
        v.name ++ " -> " ++ bindings(v.index).name
      } else {
        v.name
      }

    override def styleString = 
      if (isBound) {
        bindings(v.index).styleString
      } else "app"

    override def unfold = new Goal(super.unfold.asInstanceOf[Variable])
    override def reduce = new Goal(super.reduce.asInstanceOf[Variable])

    override def substituteAndReduce(bindings : Map[Int, Expression]) : Expression =
      if (bindings.isDefinedAt(index)) {
        bindings(index).reduce
      } else {
        new Goal(Variable(shell map (Substitution(_, bindings).reduce), index, ident, isThin))
      }

    override def toString = "Goal(" ++ v.toString ++ ")"

  }

  def refreshPreview : Unit

  def previewExpression : Expression = 
    Substitution(Filler(lift.filler.nook map (wrapVariables(_)), lift.filler.bdryIdent), bindings)

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
                    } else if(g.isThin && (! bExpr.isThin)) {
                      wksp.editor.consoleError("Match error: " ++ gExpr.toString ++ " is thin, but the expression " ++
                        bExpr.toString ++ " is not.")
                      throw new IllegalStateException
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

            // If no exception was thrown, we should have a collection of
            // compatible bindings.  Add them to the current set and refresh
            // the preview

            bindings = bindings ++ newBindings
            refreshPreview

          } catch {
            case e : IllegalStateException => {
              e.printStackTrace
              wksp.editor.consoleError("Binding failed.")
            }
          }
        }
      }
    }

}
