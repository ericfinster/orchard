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
import Identifier._

sealed trait GoalMarker
case class StableAssumption(expr : Expression) extends GoalMarker { override def toString = expr.toString }
case class DependentCell(expr : Expression) extends GoalMarker { override def toString = expr.toString }
case class FreeVariable(val variable : Variable) extends GoalMarker { override def toString = variable.toString }
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

  val bindings = HashMap.empty[String, Expression]
  val openDependents = HashMap.empty[String, Seq[String]]

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

  def renameExpression(expr : Expression) = 
    expr match {
      case Variable(ident, isThin) => Variable(translateIdent(ident), isThin)
      // Bug! - Will this be right anymore?  Do we care?
      case FillerFace(ident, filler, isThin) => FillerFace(translateIdent(ident), filler, isThin)
      case Filler(ident) => Filler(translateIdent(ident))
      case UnicityFiller(ident) => UnicityFiller(translateIdent(ident))
      case Application(_, _, _) => ???
      case Projection(_) => ???
    }

  def translateIdent(ident : Identifier) : Identifier = {
    println("Translating identifier: " ++ ident.toString)

    val idnt = Identifier(
      ident.tokens map {
        case ReferenceToken(ref) => {
          if (bindings.isDefinedAt(ref)) {
            println(ref ++ " -> " ++ bindings(ref).id)
            ReferenceToken(bindings(ref).id)
          } else {
            // throw new IllegalArgumentException("Undefined reference in identifier translation: " ++ ref)
            println("Skipping unknown identifier: " ++ ref)
            ReferenceToken(ref)
          }
        }
        case tok @ _ => tok
      }
    )

    println("Result of translation: " ++ idnt.toString)
    idnt
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

        // Tag this guys as completed and add the new cell to the environment ...
        completedDependents += depId
        openDependents -= depId

        // Add the face to the environment if appropriate
        if (dependentFramework.topCell.isFiller) {
          val ff = dependentFramework.topCell.getFillerFace.get

          ff.item foreach (e =>
            if (environment.containsId(e.id)) {
              throw new IllegalArgumentException("Identifier clash.")
            }
          )

          environment += ff.toExpressionCell
        }


        // Now add the filler
        dependentFramework.topCell.item foreach (e => 
          if (environment.containsId(e.id)) {
            throw new IllegalArgumentException("Identifier clash.")
          }
        )

        environment += dependentFramework.toExpressionCell
      }
    }
    }

    completedDependents
  }

  def isComplete : Boolean = goals.length == 0

  // There's still some unification of variables which I don't quite understand ..
  def getImports : Seq[NCell[Expression]] = {
    val imports = Buffer.empty[NCell[Expression]]

    if (isComplete) {

      val args = defn.environment.vars map (v => {
        environment.lookup(bindings(v.value.id).id).get
      })

      val framework = new Framework(shell)
      val startDim = framework.dimension
      framework.stablyAppend(Framework(defn.outputExpr))
      val endDim = framework.dimension

      val appExpr = Application(defn, args, shell)
      framework.topCell.item = Some(appExpr)

      framework forAllCells(startDim, endDim, (cell => {
        // In the middle, we need to translate the results so that they use the correct bindings
        cell.item match {
          case Some(Variable(ident, isThin)) => {
            cell.item = Some(appExpr.bindings(ident.toString))
            imports += cell.toExpressionCell
          }
          case Some(Filler(ident)) => {
            cell.item = Some(Filler(ident.translateWithBindings(appExpr.bindings)))
            imports += cell.toExpressionCell
          }
          case Some(UnicityFiller(ident)) => {
            cell.item = Some(UnicityFiller(ident.translateWithBindings(appExpr.bindings)))
            imports += cell.toExpressionCell
          }
          case Some(FillerFace(ident, filler, isThin)) => {
            // BUG! Oops, the name of the filler may be wrong now ....
            cell.item = Some(FillerFace(ident.translateWithBindings(appExpr.bindings), filler, isThin))
            imports += cell.toExpressionCell
          }
          case _ => println("Skipping here because I don't know what to do.")
        }
      }))

      imports += framework.toExpressionCell
    }

    imports
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
            case FreeVariable(v) => {
              // Make sure we only bind thin things to thin variables ...
              if (v.isThin) {
                isValid &&= testExpr.isThin
              }
                
              newBindings(v.id) = testExpr  
            }
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
          // Oh.  This needs to be more.  We should also go look at the dependent cells
          // and translate their identifiers ...
          goals foreach (g => {
            g forAllCells (cell => 
              cell.item match {
                case FreeVariable(v) => {
                  if (bindings.isDefinedAt(v.id)) {
                    cell.item = BoundVariable(v, bindings(v.id))
                  }
                }
                case DependentCell(e) => {
                  println("Renaming expression: " ++ e.toString)
                  cell.item = DependentCell(renameExpression(e))
                }
                case _ => ()
              })
          })

          // Loop on the open dependents until no more are
          // var completedDependents = importDependents

          // while (completedDependents.length > 0)
          //   completedDependents = importDependents

        } else {
          println("Expression was not valid.")
        }
      }
    }
  }
}
