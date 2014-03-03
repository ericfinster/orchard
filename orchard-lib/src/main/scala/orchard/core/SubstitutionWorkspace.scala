/**
  * SubstitutionWorkspace.scala - A workspace implementation for substitutions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.Buffer

import Environment._

sealed trait GoalMarker
case class StableAssumption(expr : Expression) extends GoalMarker
case class DependentCell(expr : Expression) extends GoalMarker
case class FreeVariable(variable : Variable) extends GoalMarker
case class BoundVariable(variable : Variable, expr : Expression) extends GoalMarker

// Maybe we need a notion of complete and incomplete dependent cell?

// Just use the simple version?  Don't seem to have anything specific here yet ...
class GoalComplex(seed : NCell[GoalMarker]) extends AbstractMutableComplex[GoalMarker](seed) {

  type CellType = GoalComplexCell

  def newCell(item : GoalMarker) = new GoalComplexCell(item)

  class GoalComplexCell(var item : GoalMarker) extends AbstractMutableCell {
    override def toString = "Cell(" ++ item.toString ++ ")@" ++ hashCode.toString
  }

}

abstract class SubstitutionWorkspace extends Workspace {

  def shell : SimpleFramework
  def defn : Definition

  def goals = openGoals.values.toSeq

  val openGoals = HashMap.empty[String, GoalComplex]
  val openDependents = HashMap.empty[String, Seq[String]]

  val bindings = HashMap.empty[String, Expression]

  // Set up the initial substitution state
  defn.environment foreach (expr => {
    expr.value match {
      case Variable(ident, _) => {

        val goalComplex = new GoalComplex(
          shell.toCell map (exprOpt => 
            exprOpt match {
              case None => null
              case Some(e) => StableAssumption(e)
            }
          )
        )

        goalComplex.stablyAppend(new GoalComplex(addMarkers(expr)))
        openGoals(ident.toString) = goalComplex
      }

      case Filler(ident) => ()
        //openDependents(ident.toString) = Seq.empty ++ defn.dependencies(SimpleFramework(expr)).keys

      case UnicityFiller(ident) => ()
        //openDependents(ident.toString) = Seq.empty ++ defn.dependencies(SimpleFramework(expr)).keys

      // We do not keep the filler faces, as we will import them simultaneously with their fillers
      case FillerFace(_, _, _) => ()
    }
  })

  def addMarkers(exprCell : NCell[Expression]) : NCell[GoalMarker] = {
    exprCell map (expr =>
      expr match {
        case v @ Variable(_, _) => FreeVariable(v)
        case e @ _ => DependentCell(e)
      }
    )
  }

  def translateIdent(ident : Identifier) : Identifier = {
    Identifier(
      ident.tokens map {
        case ReferenceToken(ref) => {
          if (bindings.isDefinedAt(ref)) {
            ReferenceToken(bindings(ref).id)
          } else {
            throw new IllegalArgumentException("Undefined reference in identifier translation.")
          }
        }
        case tok @ _ => tok
      }
    )
  }

  def importDependents : Seq[String] = {
    val completedDependents = Buffer.empty[String]

    openDependents foreach { case (depId, depSeq) => {
      if (depSeq forall (bindings.isDefinedAt(_))) {

        // Now.  Since we know *all* of its dependencies are satisfied,
        // we should be able to look up any proper face in the bindings
        // list, and all we need to do is generate the top and possible
        // filler face.

        // So what we need then is simply to map over it and grab the
        // expressions from the binding list, stopping to insert a special
        // expression when we cross the top and possible face filler.

        val dependent : NCell[Expression] =
          defn.environment.lookup(depId).get map {
            case Variable(ident, isThin) => bindings(ident.toString)
            case FillerFace(ident, filler, isThin) => {
              if (filler == depId) {
                //
                // BUG!!! - We need to recheck the value of isThin somehow ....
                //
                val newExpr = FillerFace(translateIdent(ident), filler, isThin)
                bindings(ident.toString) = newExpr
                newExpr
              } else {
                bindings(ident.toString)
              }
            }
            case Filler(ident) => {
              if (ident.toString == depId) {
                val newExpr = Filler(translateIdent(ident))
                bindings(ident.toString) = newExpr
                newExpr
              } else {
                bindings(ident.toString)
              }
            }
            case UnicityFiller(ident) => {
              if (ident.toString == depId) {
                val newExpr = Filler(translateIdent(ident))
                bindings(ident.toString) = newExpr
                newExpr
              } else {
                bindings(ident.toString)
              }
            }
          }

        // Now generate a framework and stick it in the shell
        val exprFramework = shell.clone
        exprFramework.stablyAppend(SimpleFramework(dependent))

        // Tag this guys as completed and add the new cell to the environment ...
        completedDependents += depId
        environment += exprFramework.toExpressionCell

        // BUG!!! - Before importing, we should check if there is a name clash, and
        //          if so, whether the associated cell is compatible ...
      }
    }
    }

    completedDependents
  }

  def satisfyGoal(goal : GoalComplex, expr : NCell[Expression]) : Unit = {
    goal.toCell.zip(expr) match {
      case None => println("Expression has wrong shape for this goal.")
      case Some(zippedExpr) => {
        val newBindings = HashMap.empty[String, Expression]
        val compatComplex = new SimpleMutableComplex(zippedExpr)

        var isValid = true

        compatComplex forAllCells (cell => {
          val (goalMarker, testExpr) = cell.item

          goalMarker match {
            case StableAssumption(e) => isValid &&= (e.id == testExpr.id)
            case DependentCell(e) => isValid &&= (e.id == testExpr.id)
            case FreeVariable(v) => newBindings(v.id) = testExpr
            case BoundVariable(v, e) => isValid &&= (e.id == testExpr.id)
          }
        })

        if (isValid) {

          openGoals.values foreach (og => {
            og forAllCells (cell => 
              cell.item match {
                case FreeVariable(v) => {
                  if (newBindings.isDefinedAt(v.id)) {
                    cell.item = BoundVariable(v, newBindings(v.id))
                  }
                }
                case _ => ()
              })
          })

          // Do something here for each of the bindings ..
          bindings ++= newBindings

          // Loop on the open dependents until no more are
          var completedDependents = importDependents

          while (completedDependents.length > 0)
            completedDependents = importDependents

          // Now remove the associated goals
          openGoals --= newBindings.keys
        } else {
          println("Expression did not satisfy the goal.")
        }
      }
    }
  }
}
