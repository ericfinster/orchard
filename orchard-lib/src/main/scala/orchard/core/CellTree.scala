/**
  * CellTree.scala - Trees of Opetopic Cells
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core

import scala.language.implicitConversions
import scala.collection.mutable.ListBuffer

import Nats._

sealed trait CellTree[D <: Nat, +A]

case class SeedClass[D <: Nat, +A](obj : ObjectCell[D, A])(implicit val isZero : IsZero[D]) extends CellTree[D, A]
case class LeafClass[D <: Nat, +A](shape : Cell[D#Pred, A])(implicit val hasPred : HasPred[D]) extends CellTree[D, A]
case class GraftClass[D <: Nat, +A](cell : Cell[D, A], branches : List[CellTree[D, A]])(implicit val hasPred : HasPred[D]) extends CellTree[D, A] {
  if (cell.srcTree.cellList != branches.map(t => t.output)) {
    throw new IllegalArgumentException("\nCells are not compatible:\n" ++ 
      cell.srcTree.cellList.toString ++ 
      "\n" ++ branches.map(t => t.output).toString ++ "\n")
  }

  // require(cell.srcTree.cellList == (branches map (tr => tr.output)))
}

object Seed {
  def apply[A](obj : ObjectCell[_0, A]) : SeedClass[_0, A] = SeedClass(obj)

  def apply[D <: Nat : IsZero, A](obj : Cell[D, A]) : SeedClass[_0, A] = Seed(obj.asInstanceOf[ObjectCell[_0, A]])

  def unapply[D <: Nat, A](tree : CellTree[D, A]) : Option[(ObjectCell[D, A], IsZero[D])] =
  {
    if (tree.isInstanceOf[SeedClass[D, A]]) {
      val s = tree.asInstanceOf[SeedClass[D, A]]
      Some(s.obj, s.isZero)
    } else {
      None
    }
  }
}

object Leaf {
  def apply[D <: Nat, A](shape : Cell[D, A]) : LeafClass[S[D], A] = LeafClass[S[D], A](shape)

  def unapply[D <: Nat, A](tree : CellTree[D, A]) : Option[(Cell[D#Pred, A], HasPred[D])] =
  {
    if (tree.isInstanceOf[LeafClass[D, A]]) {
      val e = tree.asInstanceOf[LeafClass[D, A]]
      Some(e.shape, e.hasPred)
    } else {
      None
    }
  }
}

object Graft {
  def apply[D <: Nat, A](cell : Cell[S[D], A], branches : List[CellTree[S[D], A]]) : GraftClass[S[D], A] =
      GraftClass(cell, branches)

  def unapply[D <: Nat, A](tree : CellTree[D, A]) : Option[(Cell[D, A], List[CellTree[D, A]], HasPred[D])] =
  {
    if (tree.isInstanceOf[GraftClass[D, A]]) {
      val i = tree.asInstanceOf[GraftClass[D, A]]
      Some(i.cell, i.branches, i.hasPred)
    } else {
      None
    }
  }
}

object CellTree {

  /*
   * I think these could be made generic using the "unapply" idea ...
   */

  implicit def toSucc[D <: Nat : HasPred, A](tree : CellTree[D, A]) : CellTree[S[D#Pred], A] =
      tree.asInstanceOf[CellTree[S[D#Pred], A]]

  implicit def toSuccList[D <: Nat : HasPred, A](l : List[CellTree[D, A]]) : List[CellTree[S[D#Pred], A]] =
      l map (t => toSucc(t)(implicitly[HasPred[D]]))

  implicit def fromSucc[D <: Nat : HasPred, A](tree : CellTree[S[D#Pred], A]) : CellTree[D, A] =
      tree.asInstanceOf[CellTree[D, A]]

  implicit def fromSuccList[D <: Nat : HasPred, A](l : List[CellTree[S[D#Pred], A]]) : List[CellTree[D, A]] =
      l map (t => fromSucc(t)(implicitly[HasPred[D]]))

  /**
    * Operations on CellTrees
    * 
    */

  implicit class CellTreeOps[D <: Nat, A](tree : CellTree[D, A]) {

    def dimension : Nat = 
      tree match {
        case Seed(obj, _) => _0
        case Leaf(shape, _) => S(shape.dimension)
        case Graft(cell, branches, _) => cell.dimension
      }

    def isLeaf : Boolean = 
      tree match {
        case Graft(_, _, _) => false
        case _ => true
      }

    def cellList : List[Cell[D, A]] =
      tree match {
        case Seed(obj, _) => obj :: Nil
        case Leaf(shape, _) => Nil
        case graft => 
          {
            val buf = new ListBuffer[Cell[D, A]]()
            graft foreach (cell => buf += cell)
            buf.toList
          }
      }

    def compareWith[B](other : CellTree[D, B], eqv : A => B => Boolean) : Boolean = 
      tree match {
        case SeedClass(to) => {
          other match {
            case SeedClass(oo) => to.compareWith(oo, eqv)
            case _ => false
          }
        }
        case LeafClass(ts) => {
          other match {
            case LeafClass(os) => ts.compareWith(os, eqv)
            case _ => false
          }
        }
        case GraftClass(tc, tbs) => {
          other match {
            case GraftClass(oc, obs) => {
              if (tc.compareWith(oc, eqv)) {
                (true /: (tbs.zip(obs) map (pr => { val (tb, ob) = pr ; tb.compareWith(ob, eqv) }))) (_&&_)
              } else false
            }
            case _ => false
          }
        }
      }

    def simultaneously[B](other : CellTree[D, B], action : A => B => Unit) : Unit = 
      tree match {
        case SeedClass(to) => {
          other match {
            case SeedClass(oo) => to.simultaneously(oo, action)
            case _ => ()
          }
        }
        case LeafClass(ts) => {
          other match {
            case LeafClass(os) => ts.simultaneously(os, action)
            case _ => ()
          }
        }
        case GraftClass(tc, tbs) => {
          other match {
            case GraftClass(oc, obs) => {
              tc.simultaneously(oc, action)
              tbs.zip(obs) foreach (pr => { val (tb, ob) = pr ; tb.simultaneously(ob, action) })
            }
            case _ => ()
          }
        }
      }

    def comultiply : CellTree[D, NCell[A]] = 
      tree match {
        case Seed(obj, ev) => {
          implicit val isZero = ev
          Seed(obj.comultiply).asInstanceOf[CellTree[D, NCell[A]]]
        }
        case Leaf(shape, ev) => {
          implicit val hasPred = ev
          Leaf(shape.comultiply)
        }
        case Graft(cell, branches, ev) => {
          implicit val hasPred = ev
          Graft(cell.comultiply, branches map (_.comultiply))
        }
      }

    def foreach(action : Cell[D, A] => Unit) : Unit =
      tree match {
        case Seed(obj, _) => action(obj)
        case Leaf(shape, _) => ()
        case Graft(cell, branches, _) =>
          {
            branches foreach (branch => branch foreach action)
            action(cell)
          }
      }

    def getUnit : CellTree[D, A] =
      tree match {
        case Graft(cell, branches, ev) =>
          {
            implicit val hasPred : HasPred[D] = ev
            LeafClass(cell.target)
          }
        case other => other
      }

    def graft[B >: A](trees : List[CellTree[D, B]]) : CellTree[D, B] =
      tree match {
        case Seed(obj, ev) => 
          {
            implicit val izZero = ev
            SeedClass(obj)
          }
        case Leaf(shape, _) => trees.head
        case graft => graft.graftLocal(trees)._1
      }

    def graftLocal[B >: A](trees : List[CellTree[D, B]]) :
        (CellTree[D, B], List[CellTree[D, B]]) =
      tree match {
        case Seed(obj, ev) => 
          {
            implicit val isZero = ev
            (SeedClass(obj), trees)
          }
        case Leaf(shape, _) => (trees.head, trees.tail)
        case Graft(cell, branches, ev) =>
          {
            implicit val hasPred = ev

            def consume(brs : List[CellTree[D, B]],
                        gbrs : List[CellTree[D, B]])
                : (List[CellTree[D, B]], List[CellTree[D, B]]) =
              brs match {
                case Nil => (Nil, gbrs)
                case b :: bs => {
                  val (newBranch, rem0) = b.graftLocal(gbrs)
                  val (remBranches, rem1) = consume(bs, rem0)
                  (newBranch :: remBranches, rem1)
                }
              }

            val (newBranches, rem) = consume(branches, trees)

            (GraftClass(cell, newBranches), rem)
          }
      }

    def substitute[B >: A](trees : List[CellTree[S[D], B]]) : CellTree[D, B] =
      tree match {
        case Seed(obj, _) => trees.head.leaves
        case Leaf(shape, ev) => 
          {
            implicit val hasPred = ev
            LeafClass(shape)
          }
        case graft => graft.substituteLocal(trees)._1
      }

    def substituteLocal[B >: A](trees : List[CellTree[S[D], B]])
        : (CellTree[D, B], List[CellTree[S[D] , B]]) =
      tree match {
        case Seed(obj, _) => (trees.head.leaves, trees.tail)
        case Leaf(shape, ev) => 
          {
            implicit val hasPred = ev
            (LeafClass(shape), trees)
          }
        case Graft(cell, branches, ev) => 
          {
            implicit val hasPred = ev

            def consume(brs : List[CellTree[D, A]],
                        trs : List[CellTree[S[D], B]])
                : (List[CellTree[D, B]], List[CellTree[S[D], B]]) = brs match {
                case Nil => (Nil, trs)
                case b :: bs => {
                  val (t, rem0) = b.substituteLocal(trs)
                  val (ts, rem1) = consume(bs, rem0)
                  (t :: ts, rem1)
                }
              }

            val (newBranches, rem) = consume(branches, trees)
            val newNode = rem.head.leaves

            (newNode.graft(newBranches), rem.tail)
          }
      }

    def leafList(implicit hasPred : HasPred[D]) : List[Cell[D#Pred, A]] =
      tree match {
        case Leaf(shape, _) => shape :: Nil
        case Graft(cell, branches, _) => branches flatMap (_.leafList)
      }

    def leaves(implicit hasPred : HasPred[D]) : CellTree[D#Pred, A] = 
        tree match {
          case Leaf(shape, _) => shape.corolla
          case Graft(cell, branches, _) =>
            cell.srcTree.substitute(branches)
        }

    def target[B >: A](tgtValue : B) : Cell[D, B] =
        tree match {
          case Seed(obj, ev) => 
            {
              implicit val isZero = ev
              ObjectCell(tgtValue)
            }
          case Leaf(shape, ev) => 
            {
              implicit val hasPred = ev
              CompositeCell(tgtValue, output.corolla, output.value)
            }
          case Graft(cell, branches, ev) =>
            {
              implicit val hasPred = ev
              CompositeCell(tgtValue, tree.leaves, cell.target.value)
            }
        }

    def output(implicit hasPred : HasPred[D]) : Cell[D#Pred, A] =
        tree match {
          case Leaf(shape, _) => shape
          case Graft(cell, branches, _) => cell.target
        }

    def regenerateFrom[T >: A, B](generator : CellRegenerator[T, B], 
                                  lvs : CellTree[D#Pred, B])(implicit hasPred : HasPred[D])
        : CellTree[D, B] =
      regenerateFromLocal(generator, lvs)._1

    def regenerateFromLocal[T >: A, B](generator : CellRegenerator[T, B], 
                                       hRoot : CellTree[D#Pred, B])(implicit hasPred : HasPred[D])
        : (CellTree[D, B],                // The completed cell tree
           List[CellTree[D#Pred, B]]) =   // The outgoing trees left from fracturing hRoot
      tree match {
        case Leaf(shape, _) => 
          hRoot match {
            // The case of a leaf is a match error: there must be a cell
            // of the previous dimension corresponding to this leaf
            case Seed(obj, _) => (LeafClass(obj), Nil)
            case Graft(cell, branches, e) => (LeafClass(cell), branches)
          }
        case Graft(cell, branches, _) =>
          {
            if (branches == Nil) {
              val newCell = 
                generator.generateCell(cell.value,
                                       hRoot.getUnit,
                                       cell.targetValue)

              return (GraftClass(newCell, Nil), hRoot :: Nil)
            }

            var remainingVertical : ListBuffer[CellTree[D, A]] = new ListBuffer() ++ branches
            var outgoingTrees : List[CellTree[D#Pred, B]] = Nil

            def trace(srcTree : CellTree[D#Pred, A], myRoot : CellTree[D#Pred, B])
                : (CellTree[D#Pred, B],        // The newly created source tree
                   List[CellTree[D, B]]) =     // The vertical trees which cover it
            {
              var childVBranches : ListBuffer[CellTree[D, B]] = new ListBuffer()

              val (finishedBranch, outgoingH) =
                remainingVertical.last.regenerateFromLocal(generator, myRoot)
              // Now drop the used items ....
              remainingVertical = remainingVertical.init

              def childConsumer(pr : (CellTree[D#Pred, A], CellTree[D#Pred, B])) : CellTree[D#Pred, B] = {
                val (child, itsRoot) = pr

                if (child.isLeaf) {
                  outgoingTrees ::= itsRoot
                  itsRoot.getUnit
                } else {
                  val (mySrcTree, myVerticalBranches) = trace(child, itsRoot) // We should trace the subtree ..
                  childVBranches = myVerticalBranches ++: childVBranches      // Now add the resulting branches to our list ...
                  mySrcTree                                                   // And return the resulting tree ...
                }
              }

              val children = srcTree match {
                  case Graft(_, branches, _) => branches
                  case other => other :: Nil
                }

              val newSrcBranches = children.zip(outgoingH).reverse map childConsumer
              (finishedBranch.output.corolla.graft(newSrcBranches.reverse), (childVBranches += finishedBranch).toList)
            }
            
            val (newSrc, newBranches) = trace(cell.srcTree, hRoot)
            (GraftClass(generator.generateCell(cell.value, newSrc, cell.targetValue), newBranches), outgoingTrees)
          }
      }
  }
}
