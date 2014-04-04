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

sealed trait SubstIndex
case class Internal(seq : Seq[Int]) extends SubstIndex
case class External(seq : Seq[Int]) extends SubstIndex

abstract class SubstitutionWorkspace extends Workspace with Context[SubstIndex] { thisWksp =>

  type IndexType = SubstIndex

  implicit def indexHasEmpty : HasEmpty[SubstIndex] = 
    new HasEmpty[SubstIndex] {
      def empty = External(Seq.empty)
    }

  def parentDefinition : DefinitionWorkspace

  def convertIndex(idx : Workspace#IndexType) : IndexType = {
    if (idx.isInstanceOf[SubstIndex]) {
      idx.asInstanceOf[SubstIndex]
    } else {
      External(idx.asInstanceOf[Seq[Int]])
    }
  }

  def promoteInternalExpr(expr : Expression[Seq[Int]]) : Expression[SubstIndex] = expr map (Internal(_))
  def promoteExternalExpr(expr : Expression[Seq[Int]]) : Expression[SubstIndex] = expr map (External(_))
  def demoteExpr(expr : Expression[SubstIndex]) : Expression[Seq[Int]] = 
    expr map (idx => 
      idx match {
        case External(seq) => seq
        case Internal(_) => throw new IllegalArgumentException("Internal reference in demotion.")
      }
    )

  def get(idx : SubstIndex) : Option[Expression[SubstIndex]] = 
    idx match {
      case Internal(seq) => defn.context.get(seq) map (promoteInternalExpr(_))
      case External(seq) => parentDefinition.context.get(seq) map (promoteExternalExpr(_))
    }

  def indexOf(expr : Expression[SubstIndex]) : SubstIndex = ???

  def extendWith(expr : Expression[SubstIndex]) : SubstIndex = {
    External(parentDefinition.context.extendWith(demoteExpr(expr)))
  }

  def nextIndex : SubstIndex = External(parentDefinition.context.nextIndex)
  def nextIndexPair : (SubstIndex, SubstIndex) = {
    val (fst, snd) = parentDefinition.context.nextIndexPair
    (External(fst), External(snd))
  }

  def freeVariableMap : Map[String, SubstIndex] = {
    // Uhhh ... is there a better way to do this ????
    Map.empty[String, SubstIndex] ++= parentDefinition.context.freeVariableMap mapValues (External(_))
  }

  def defn : Definition
  def shell : NCell[Seq[Int]]
  def context = thisWksp

  val application = Application(defn, Seq.empty, shell)

  class SubstitutionWorksheet(seed : NCell[Polarity[SubstIndex]]) 
      extends Worksheet(seed) {

    type CellType = SubstitutionWorksheetCell

    def newCell(itm : Polarity[SubstIndex]) = new SubstitutionWorksheetCell(itm)
    def extract(cell : SubstitutionWorksheetCell) = new SubstitutionWorksheet(cell.toNCell)
    
    // This is where we look it up in the context
    def getExpression(pol : Polarity[SubstIndex]) : Option[Expression[SubstIndex]] = 
      pol match {
        case Neutral(idx) => get(idx)
        case _ => None
      }

    class SubstitutionWorksheetCell(itm : Polarity[SubstIndex]) extends WorksheetCell(itm)

  }


  // def isComplete : Boolean = {

  //   var isCmplt : Boolean = true

  //   for {
  //     i <- Range(0, defn.context.length)
  //   } {
  //     defn.context(i) match {
  //       case Variable(_, _, _) => 
  //         isCmplt &&= (application.bindings.isDefinedAt(i))
  //       case _ => ()
  //     }
  //   }

  //   isCmplt

  // }

  // sealed trait GoalAddr
  // case class Internal(seq : Seq[Int]) extends GoalAddr
  // case class External(seq : Seq[Int]) extends GoalAddr

  // implicit val goalAddrIsExpressionLike : FrameworkInhabitant[GoalAddr] =
  //   new FrameworkInhabitant[GoalAddr] {
  //     def toExpression(ga : GoalAddr) = 
  //       ga match {
  //         case Internal(seq) => { println("Looking up internal: " ++ seq.toString) ; defn.project(seq) }
  //         case External(seq) => { println("Looking up external: " ++ seq.toString) ; project(seq) }
  //       }

  //     def empty : GoalAddr = External(Seq.empty)
  //   }

  // class GoalFramework(seed : NCell[GoalAddr]) 
  //     extends Framework[GoalAddr](seed)
  //     with CheckableFramework[GoalAddr] { thisFramework =>

  //   type CellType = GoalFrameworkCell

  //   def newCell(item : GoalAddr) = new GoalFrameworkCell(item)
  //   def extract(cell : GoalFrameworkCell) = new GoalFramework(cell.toNCell)

  //   class GoalFrameworkCell(item : GoalAddr) extends FrameworkCell(item) with CheckableCell

  // }

  // So, we project internally and externally.  Now what?  We're going to need to keep a list
  // still of the bindings that we make.

  // def initialize = {
  //   println("Definition context length: " ++ defn.context.length.toString)

  //   // Set up the initial substitution state
  //   for {
  //     i <- Range(0, defn.context.length)
  //   } {
  //     println("processing item: " ++ i.toString)
  //     defn.context(i) match {
  //       case Variable(varShell, ident, _) => {
  //         println("Found a variable: " ++ ident.toString)

  //         val goalComplex = new GoalFramework(
  //           shell.ncell map (seq => External(seq))
  //         )

  //         println("Completed initial complex")

  //         goalComplex.stablyAppend(
  //           new GoalFramework(varShell.ncell map (seq => Internal(seq)))
  //         )

  //         println("Completed append.")

  //         goalComplex.topCell.item = Internal(Seq(i))

  //         goals += goalComplex
  //       }

  //       case _ => ()
  //     }
  //   }
  // }

  // def satisfyGoal(goal : GoalFramework, expr : ShapeFramework) : Unit = {
  //   goal.toCell.zip(expr.toCell) match {
  //     case None => println("Expression has wrong shape for this goal.")
  //     case Some(zippedShape) => {

  //       var isValid = true

  //       zippedShape map {
  //         case (Internal(goalSeq), exprSeq) => {
  //           // The question is what to do here...

  //           // If the internal pointed is to a variable, then no problem, we know what to do:
  //           // we are just going to tag the expression sequence as a new binding and move on.

  //           // On the other hand, if it points to a derived internal cell, we have to do some
  //           // checking to make sure it is compatible.  What does that checking entail?

  //           // Ah, okay, no I see.  In fact, the sequence really *should* point to the internal
  //           // cell as well, indicating that we have used the hypothetically generated cell in 
  //           // a diagram for which we are trying to get to the next goal.  I see.

  //           // But now this brings up the following question: where, in the current setup am I
  //           // able to access these dependent cells, since clearly I will be needing them ...

  //           // And this is definitely lacking.

  //           // Right, and when we add the cell to a worksheet, it should appear in its transformed
  //           // state. That is, with the stable assumptions added and with all variables translated
  //           // as they will be in the final state.

  //           // How to accomplish this?

            

  //           ???
  //         }
  //         case (External(goalSeq), exprSeq) => isValid &&= (goalSeq == exprSeq)
  //       }
  //     }
  //   }
  // }

  // def satisfyGoal(goal : GoalComplex, expr : NCell[Expression]) : Unit = {
  //   goal.toCell.zip(expr) match {
  //     case None => println("Expression has wrong shape for this goal.")
  //     case Some(zippedExpr) => {
  //       val newBindings = HashMap.empty[String, Expression]
  //       val compatComplex = new SimpleMutableComplex(zippedExpr)

  //       var isValid = true

  //       compatComplex forAllCells (cell => {
  //         val (goalMarker, testExpr) = cell.item

  //         goalMarker match {
  //           case StableAssumption(e) => isValid &&= (e.id == testExpr.id)
  //           case DependentCell(e) => isValid &&= (e.id == testExpr.id)
  //           case FreeVariable(v) => {
  //             // Make sure we only bind thin things to thin variables ...
  //             if (v.isThin) {
  //               isValid &&= testExpr.isThin
  //             }
                
  //             newBindings(v.id) = testExpr  
  //           }
  //           case BoundVariable(v, e) => isValid &&= (e.id == testExpr.id)
  //         }
  //       })

  //       if (isValid) {

  //         newBindings foreach { 
  //           case (str, expr) => {
  //             goals.lookup(str) foreach (g => goals -= g)
  //             bindings(str) = expr
  //           }
  //         }

  //         // Update the goal complexes ...
  //         // Oh.  This needs to be more.  We should also go look at the dependent cells
  //         // and translate their identifiers ...
  //         goals foreach (g => {
  //           g forAllCells (cell => 
  //             cell.item match {
  //               case FreeVariable(v) => {
  //                 if (bindings.isDefinedAt(v.id)) {
  //                   cell.item = BoundVariable(v, bindings(v.id))
  //                 }
  //               }
  //               case DependentCell(e) => {
  //                 println("Renaming expression: " ++ e.toString)
  //                 cell.item = DependentCell(renameExpression(e))
  //               }
  //               case _ => ()
  //             })
  //         })

  //         // Loop on the open dependents until no more are
  //         // var completedDependents = importDependents

  //         // while (completedDependents.length > 0)
  //         //   completedDependents = importDependents

  //       } else {
  //         println("Expression was not valid.")
  //       }
  //     }
  //   }
  // }


  // sealed trait GoalMarker
  // case class StableAssumption(externalAddr : Seq[Int]) extends GoalMarker
  // case class DependentCell(internalAddr : Seq[Int]) extends GoalMarker
  // case class FreeVariable(internalIndex : Int) extends GoalMarker
  // case class BoundVariable(internalIndex : Int, externalIndex : Seq[Int]) extends GoalMarker

  // class GoalComplex(seed : NCell[GoalMarker]) extends AbstractMutableComplex[GoalMarker](seed) {

  //   type CellType = GoalComplexCell

  //   def newCell(item : GoalMarker) = new GoalComplexCell(item)

  //   class GoalComplexCell(var item : GoalMarker) extends AbstractMutableCell {
  //     override def toString = "Cell(" ++ item.toString ++ ")@" ++ hashCode.toString
  //   }

  // }


  // def addMarkers(exprCell : NCell[Expression]) : NCell[GoalMarker] = {
  //   exprCell map (expr =>
  //     expr match {
  //       case v @ Variable(_, _) => FreeVariable(v)
  //       case e @ _ => DependentCell(e)
  //     }
  //   )
  // }

  // def renameExpression(expr : Expression) = 
  //   expr match {
  //     case Variable(ident, isThin) => Variable(translateIdent(ident), isThin)
  //     // Bug! - Will this be right anymore?  Do we care?
  //     case FillerFace(ident, filler, isThin) => FillerFace(translateIdent(ident), filler, isThin)
  //     case Filler(ident) => Filler(translateIdent(ident))
  //     case UnicityFiller(ident) => UnicityFiller(translateIdent(ident))
  //     case Application(_, _, _) => ???
  //     case Projection(_) => ???
  //   }

  // def translateIdent(ident : Identifier) : Identifier = {
  //   println("Translating identifier: " ++ ident.toString)

  //   val idnt = Identifier(
  //     ident.tokens map {
  //       case ReferenceToken(ref) => {
  //         if (bindings.isDefinedAt(ref)) {
  //           println(ref ++ " -> " ++ bindings(ref).id)
  //           ReferenceToken(bindings(ref).id)
  //         } else {
  //           // throw new IllegalArgumentException("Undefined reference in identifier translation: " ++ ref)
  //           println("Skipping unknown identifier: " ++ ref)
  //           ReferenceToken(ref)
  //         }
  //       }
  //       case tok @ _ => tok
  //     }
  //   )

  //   println("Result of translation: " ++ idnt.toString)
  //   idnt
  // }

  // def importDependents : Seq[String] = {
  //   val completedDependents = Buffer.empty[String]

  //   openDependents foreach { case (depId, depSeq) => {
  //     if (depSeq forall (bindings.isDefinedAt(_))) {

  //       println("Completed dependent: " ++ depId)

  //       // First we build a framework for the new dependent
  //       val dependentFramework = new Framework(shell)
  //       val startDim = dependentFramework.dimension  
  //       dependentFramework.stablyAppend(Framework(defn.environment.lookup(depId).get))
  //       val endDim = dependentFramework.dimension + 1

  //       def translateIds = 
  //         dependentFramework forAllCells(startDim, endDim, 
  //           (cell => {
  //             cell.item foreach (e => {
  //               if (bindings.isDefinedAt(e.id)) {
  //                 cell.item = Some(bindings(e.id))
  //               } else {
  //                 println("Missing identifier " ++ e.id ++ " in translation.")
  //               }
  //             })
  //           })
  //         )

  //       if (dependentFramework.topCell.isFiller) {
  //         // A filler which as a face
  //         val filler = dependentFramework.topCell
  //         val ff = dependentFramework.topCell.getFillerFace.get

  //         // Save the items for use later
  //         val fillerItem = filler.item.get.asInstanceOf[Filler]
  //         val ffItem = ff.item.get.asInstanceOf[FillerFace]

  //         // Clear the parts which will be replaced
  //         filler.item = None
  //         ff.item = None

  //         // Translate the rest via the bindings
  //         translateIds

  //         // At this point, we should have an exposed nook corresponding to the
  //         // lift which this depenedent represents
  //         if (filler.isExposedNook) {
  //           val newFillerItem = Filler(translateIdent(fillerItem.ident))
  //           val newFfItem = FillerFace(translateIdent(ffItem.ident), newFillerItem.id, filler.isThinFillerFace)

  //           filler.item = Some(newFillerItem)
  //           ff.item = Some(newFfItem)

  //           // Now we need to set the bindings ...
  //           bindings(fillerItem.id) = newFillerItem
  //           bindings(ffItem.id) = newFfItem

  //         } else {
  //           throw new IllegalArgumentException("After translation, we don't have a nook!")
  //         }
  //       } else {
  //         // A unicity filler
  //         val filler = dependentFramework.topCell
  //         val fillerItem = filler.item.get.asInstanceOf[UnicityFiller]
  //         filler.item = None

  //         translateIds

  //         if (filler.isUnicityFillable) {
  //           val newFillerItem = UnicityFiller(translateIdent(fillerItem.ident))
  //           filler.item = Some(newFillerItem)
  //           bindings(fillerItem.id) = newFillerItem
  //         } else {
  //           throw new IllegalArgumentException("After translate, we are not unicity fillable!")
  //         }
  //       }

  //       // Tag this guys as completed and add the new cell to the environment ...
  //       completedDependents += depId
  //       openDependents -= depId

  //       // Add the face to the environment if appropriate
  //       if (dependentFramework.topCell.isFiller) {
  //         val ff = dependentFramework.topCell.getFillerFace.get

  //         ff.item foreach (e =>
  //           if (environment.containsId(e.id)) {
  //             throw new IllegalArgumentException("Identifier clash.")
  //           }
  //         )

  //         environment += ff.toExpressionCell
  //       }


  //       // Now add the filler
  //       dependentFramework.topCell.item foreach (e => 
  //         if (environment.containsId(e.id)) {
  //           throw new IllegalArgumentException("Identifier clash.")
  //         }
  //       )

  //       environment += dependentFramework.toExpressionCell
  //     }
  //   }
  //   }

  //   completedDependents
  // }

  // def isComplete : Boolean = goals.length == 0

  // // There's still some unification of variables which I don't quite understand ..
  // def getImports : Seq[NCell[Expression]] = {
  //   val imports = Buffer.empty[NCell[Expression]]

  //   if (isComplete) {

  //     val args = defn.environment.vars map (v => {
  //       environment.lookup(bindings(v.value.id).id).get
  //     })

  //     val framework = new Framework(shell)
  //     val startDim = framework.dimension
  //     framework.stablyAppend(Framework(defn.outputExpr))
  //     val endDim = framework.dimension

  //     val appExpr = Application(defn, args, shell)
  //     framework.topCell.item = Some(appExpr)

  //     framework forAllCells(startDim, endDim, (cell => {
  //       // In the middle, we need to translate the results so that they use the correct bindings
  //       cell.item match {
  //         case Some(Variable(ident, isThin)) => {
  //           cell.item = Some(appExpr.bindings(ident.toString))
  //           imports += cell.toExpressionCell
  //         }
  //         case Some(Filler(ident)) => {
  //           cell.item = Some(Filler(ident.translateWithBindings(appExpr.bindings)))
  //           imports += cell.toExpressionCell
  //         }
  //         case Some(UnicityFiller(ident)) => {
  //           cell.item = Some(UnicityFiller(ident.translateWithBindings(appExpr.bindings)))
  //           imports += cell.toExpressionCell
  //         }
  //         case Some(FillerFace(ident, filler, isThin)) => {
  //           // BUG! Oops, the name of the filler may be wrong now ....
  //           cell.item = Some(FillerFace(ident.translateWithBindings(appExpr.bindings), filler, isThin))
  //           imports += cell.toExpressionCell
  //         }
  //         case _ => println("Skipping here because I don't know what to do.")
  //       }
  //     }))

  //     imports += framework.toExpressionCell
  //   }

  //   imports
  // }

}
