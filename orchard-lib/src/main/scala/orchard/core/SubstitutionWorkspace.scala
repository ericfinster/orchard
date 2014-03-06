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
case class StableAssumption(expr : Expression) extends GoalMarker { override def toString = expr.toString }
case class DependentCell(expr : Expression) extends GoalMarker { override def toString = expr.toString }
case class FreeVariable(variable : Variable) extends GoalMarker { override def toString = variable.toString }
case class BoundVariable(variable : Variable, expr : Expression) extends GoalMarker { override def toString = expr.toString }

class GoalComplex(seed : NCell[GoalMarker]) extends AbstractMutableComplex[GoalMarker](seed) {

  type CellType = GoalComplexCell

  def newCell(item : GoalMarker) = new GoalComplexCell(item)

  class GoalComplexCell(var item : GoalMarker) extends AbstractMutableCell {
    override def toString = "Cell(" ++ item.toString ++ ")@" ++ hashCode.toString
  }

}

abstract class SubstitutionWorkspace extends Workspace {

  type GoalSeqType <: Buffer[GoalComplex]

  // This should be changed to a framework in the current workspace.
  def defn : Definition
  def goals : GoalSeqType
  def shell : NCell[Option[Expression]]

  val openDependents = HashMap.empty[String, Seq[String]]

  val bindings = HashMap.empty[String, Expression]

  implicit class GoalOps(gls : Seq[GoalComplex]) {

    def lookup(str : String) : Option[GoalComplex] = 
      gls find (g => g.topCell.item.toString == str)

  }

  def initialize = {
    // Set up the initial substitution state
    defn.environment foreach (expr => {
      expr.value match {
        case Variable(ident, _) => {

          val goalComplex = new GoalComplex(
            shell map (exprOpt =>
              exprOpt match {
                case None => null
                case Some(e) => StableAssumption(e)
              }
            )
          )

          goalComplex.stablyAppend(new GoalComplex(addMarkers(expr)))
          goals += goalComplex
        }

        // Only fillers will count as dependents, and we'll use them as if they were lifts ...
        case f @ Filler(_) => openDependents(f.id) = Framework(expr).dependencies(defn.environment) map (_.value.id)
        case f @ UnicityFiller(_) => openDependents(f.id) = Framework(expr).dependencies(defn.environment) map (_.value.id)
        case _ => ()
      }
    })
  }

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

        println("Completed dependent: " ++ depId)

        // First we build a framework for the new dependent
        val dependentFramework = new Framework(shell)
        val startDim = dependentFramework.dimension  
        dependentFramework.stablyAppend(Framework(defn.environment.lookup(depId).get))
        val endDim = dependentFramework.dimension + 1

        def translateIds = 
          dependentFramework forAllCells(startDim, endDim, 
            (cell => {
              cell.item foreach (e => {
                if (bindings.isDefinedAt(e.id)) {
                  cell.item = Some(bindings(e.id))
                } else {
                  println("Missing identifier " ++ e.id ++ " in translation.")
                }
              })
            })
          )

        if (dependentFramework.topCell.isFiller) {
          // A filler which as a face
          val filler = dependentFramework.topCell
          val ff = dependentFramework.topCell.getFillerFace.get

          // Save the items for use later
          val fillerItem = filler.item.get.asInstanceOf[Filler]
          val ffItem = ff.item.get.asInstanceOf[FillerFace]

          // Clear the parts which will be replaced
          filler.item = None
          ff.item = None

          // Translate the rest via the bindings
          translateIds

          // At this point, we should have an exposed nook corresponding to the
          // lift which this depenedent represents
          if (filler.isExposedNook) {
            val newFillerItem = Filler(translateIdent(fillerItem.ident))

            println("New filler name: " ++ newFillerItem.id)

            val newFfItem = FillerFace(translateIdent(ffItem.ident), newFillerItem.id, filler.isThinFillerFace)

            filler.item = Some(newFillerItem)
            ff.item = Some(newFfItem)

            // Now we need to set the bindings ...
            bindings(fillerItem.id) = newFillerItem
            bindings(ffItem.id) = newFfItem

          } else {
            throw new IllegalArgumentException("After translation, we don't have a nook!")
          }
        } else {
          // A unicity filler
          val filler = dependentFramework.topCell
          val fillerItem = filler.item.get.asInstanceOf[UnicityFiller]
          filler.item = None

          translateIds

          if (filler.isUnicityFillable) {
            val newFillerItem = UnicityFiller(translateIdent(fillerItem.ident))
            filler.item = Some(newFillerItem)
            bindings(fillerItem.id) = newFillerItem
          } else {
            throw new IllegalArgumentException("After translate, we are not unicity fillable!")
          }
        }

        println("After update, topCell is: " ++ dependentFramework.topCell.item.toString)

        // Tag this guys as completed and add the new cell to the environment ...
        completedDependents += depId
        openDependents -= depId

        // Add the face to the environment if appropriate
        if (dependentFramework.topCell.isFiller) {
          val ff = dependentFramework.topCell.getFillerFace.get

          ff.item foreach (e =>
            if (environment.contains(e.id)) {
              throw new IllegalArgumentException("Identifier clash.")
            }
          )

          environment += ff.toExpressionCell
        }


        // Now add the filler
        dependentFramework.topCell.item foreach (e => 
          if (environment.contains(e.id)) {
            throw new IllegalArgumentException("Identifier clash.")
          }
        )

        environment += dependentFramework.toExpressionCell
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

          newBindings foreach { 
            case (str, expr) => {
              goals.lookup(str) foreach (g => goals -= g)
              bindings(str) = expr
            }
          }

          // Update the goal complexes ...
          goals foreach (g => {
            g forAllCells (cell => 
              cell.item match {
                case FreeVariable(v) => {
                  if (bindings.isDefinedAt(v.id)) {
                    cell.item = BoundVariable(v, bindings(v.id))
                  }
                }
                case _ => ()
              })
          })

          // Loop on the open dependents until no more are
          var completedDependents = importDependents

          while (completedDependents.length > 0)
            completedDependents = importDependents

        } else {
          println("Expression was not valid.")
        }
      }
    }
  }
}
