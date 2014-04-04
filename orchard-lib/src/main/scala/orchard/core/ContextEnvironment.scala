/**
  * ContextEnvironment.scala - Routines implementable with a context available
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.Buffer

trait Context[A] {

  def get(a : A) : Option[Expression[A]]
  def indexOf(expr : Expression[A]) : A
  def extendWith(expr : Expression[A]) : A
  def nextIndex : A
  def nextIndexPair : (A, A)

  // Add here a method to lookup by name as well ....
  // And a method to retrieve the free variables ....
  // And perhaps a method to traverse the context ....

  def freeVariableMap : Map[String, A] 

  def expandIdentifier(ident : IndexedIdentifier[A]) : String = {
    val strs = 
      ident.tokens map {
        case StringToken(str) => str
        case IndexToken(idx) => expandIdentifier(get(idx).get.ident)
      }

    strs.mkString
  }

}

trait ContextEnvironment extends CheckableEnvironment {

  type IndexType

  implicit def indexHasEmpty : HasEmpty[IndexType]

  def context : Context[IndexType]

  class ShapeFramework(seed : NCell[IndexType])
      extends Framework[IndexType](seed)
      with CheckableFramework[IndexType] { thisFramework =>

    type CellType = ShapeFrameworkCell
    type ExprIndexType = IndexType

    def newCell(item : IndexType) = new ShapeFrameworkCell(item)
    def extract(cell : ShapeFrameworkCell) = new ShapeFramework(cell.toNCell)
    def getExpression(idx : IndexType) : Option[Expression[IndexType]] = context.get(idx)

    class ShapeFrameworkCell(item : IndexType) extends FrameworkCell(item) with CheckableCell

  }

  object ShapeFramework {
    def apply(idx : IndexType) : ShapeFramework = {
      context.get(idx).get match {
        case Variable(shell, _, _) => {
          val framework = new ShapeFramework(shell)
          framework.topCell.item = idx
          framework
        }
        case Interior(bdryIdx, nook, _) => {
          val framework = new ShapeFramework(nook)
          framework.topCell.boundaryFace.item = bdryIdx
          framework.topCell.item = idx
          framework
        }
        case Boundary(intrIdx, _, _) => {
          val interior = context.get(intrIdx).get.asInstanceOf[Interior[IndexType]]
          val framework = new ShapeFramework(interior.nook)
          val bdryFramework = framework.extract(framework.topCell.boundaryFace)
          bdryFramework.topCell.item = idx
          bdryFramework
        }
        case Contraction(shell, _) => {
          val framework = new ShapeFramework(shell)
          framework.topCell.item = idx
          framework
        }
        case Application(_, _, _) => ???
      }
    }
  }
}

trait SequentialContextEnvironment extends ContextEnvironment with Context[Seq[Int]] {

  type IndexType = Seq[Int]

  override implicit def indexHasEmpty = HasEmpty.seqHasEmpty[Int]

  def context = this
  def contextBuffer : Buffer[Expression[Seq[Int]]]

  def freeVariableMap : Map[String, Seq[Int]] = {
    val fvMap = HashMap.empty[String, Seq[Int]]

    for {
      i <- Range(0, contextBuffer.length)
    } {
      contextBuffer(i) match {
        case Variable(_, ident, _) => fvMap(expandIdentifier(ident)) = Seq(i)
        case _ => ()
      }
    }

    fvMap
  }

  def get(seq : Seq[Int]) : Option[Expression[Seq[Int]]] = {
    // This seems to be where the rewriting work needs to go now. Hmm ...
    if (seq.length == 0) {
      None
    } else if (seq.length > 1) {
      val idx = seq.head

      contextBuffer(idx) match {
        case Application(defn, bindings, appShell) => {

          val baseComplex : ShapeFramework = new ShapeFramework(appShell)
          val localExpr = defn.get(seq.tail).get

          localExpr match {
            // Hmm.  I think, yes, we will need to externalize variables as well, since they
            // are going to be, essentially the root of this whole big tree.  And, for example
            // when we import definitions which terminate in a variable.
            case Variable(varShell, ident, isThin) => {
              val shellFramework = new ShapeFramework(varShell map (idx +: _))
              baseComplex.stablyAppend(shellFramework)
              Some(Variable(baseComplex.toCell, ???, isThin))
            }
            case Interior(boundaryIndex, nook, ident) => {
              // I think ident needs to be changed so that it's references hold integers picking
              // out the cells whose name's we want to write into in the environment.
              val nookFramework = new ShapeFramework(nook map (idx +: _))
              baseComplex.stablyAppend(nookFramework)
              assert(baseComplex.topCell.isExposedNook)
              Some(Interior(idx +: boundaryIndex, baseComplex.toCell, ???))
            }
            case Boundary(interiorIndex, ident, isThin) => {
              get(interiorIndex) match {
                case Some(Interior(_, nook, _)) => {
                  // So, this nook *should* be already in the correct coordinates
                  val intrFramework = new ShapeFramework(nook)
                  Some(Boundary(idx +: interiorIndex, ???, intrFramework.topCell.isThinFillerFace))
                }
                case _ => throw new IllegalArgumentException("Failed to find interior of boundary cell.")
              }
            }
            case Contraction(contrShell, ident) => {
              val shellFramework = new ShapeFramework(contrShell map (idx +: _))
              baseComplex.stablyAppend(shellFramework)
              assert(baseComplex.topCell.isUnicityFillable)
              Some(Contraction(baseComplex.toCell, ???))
            }
            case Application(_, _, _) => ???
          }
        }
        case _ => throw new IllegalArgumentException("Index head points to non-application")
      }
    } else {
      Some(contextBuffer(seq.head))
    }
  }

  def indexOf(expr : Expression[Seq[Int]]) : Seq[Int] =
    Seq(contextBuffer.indexOf(expr))

  def extendWith(expr : Expression[Seq[Int]]) : Seq[Int] = {
    val newIdx = nextIndex
    contextBuffer += expr
    newIdx
  }

  def nextIndex : Seq[Int] = Seq(contextBuffer.length)

  def nextIndexPair : (Seq[Int], Seq[Int]) = {
    val len = contextBuffer.length
    (Seq(len), Seq(len + 1))
  }

}
