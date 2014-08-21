/**
  * CellTree.scala - Trees of Opetopic Cells
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core.cell

import scala.language.implicitConversions

import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

import orchard.core.util._

import Nats._
import Util._

sealed trait CellTree[D <: Nat, +A]

case class SeedClass[D <: Nat, +A](obj : ObjectCell[D, A])(implicit val isZero : IsZero[D]) extends CellTree[D, A]
case class LeafClass[D <: Nat, +A](shape : Cell[D#Pred, A])(implicit val hasPred : HasPred[D]) extends CellTree[D, A]
case class GraftClass[D <: Nat, +A](cell : Cell[D, A], branches : Vector[CellTree[D, A]])(implicit val hasPred : HasPred[D]) extends CellTree[D, A] {

  if (cell.srcTree.cells != (branches map (_.output))) {
    throw new IllegalArgumentException("\nCells are not compatible:\n" ++ 
      cell.srcTree.cells.toString ++ 
      "\n" ++ branches.map(t => t.output).toString ++ "\n")
  }

  // require(cell.srcTree.cells == (branches map (_.output)))

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
  def apply[D <: Nat, A](cell : Cell[S[D], A], branches : Vector[CellTree[S[D], A]]) : GraftClass[S[D], A] =
      GraftClass(cell, branches)

  def unapply[D <: Nat, A](tree : CellTree[D, A]) : Option[(Cell[D, A], Vector[CellTree[D, A]], HasPred[D])] =
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

  implicit def toSuccVect[D <: Nat : HasPred, A](l : Vector[CellTree[D, A]]) : Vector[CellTree[S[D#Pred], A]] =
      l map (t => toSucc(t)(implicitly[HasPred[D]]))

  implicit def fromSucc[D <: Nat : HasPred, A](tree : CellTree[S[D#Pred], A]) : CellTree[D, A] =
      tree.asInstanceOf[CellTree[D, A]]

  implicit def fromSuccVect[D <: Nat : HasPred, A](l : Vector[CellTree[S[D#Pred], A]]) : Vector[CellTree[D, A]] =
      l map (t => fromSucc(t)(implicitly[HasPred[D]]))

  /*
   * Conversions to RoseTrees
   */

  def toRoseTree[D <: Nat, A](t : CellTree[D, A], isZero : IsZero[D]) : RoseTree[Cell[D, A], Unit] =
    t match {
      case Seed(obj, _) => Branch(obj, Vector(Rose(())))
    }

  def toRoseTree[D <: Nat, A](t : CellTree[D, A], hasPred : HasPred[D]) : RoseTree[Cell[D, A], Cell[D#Pred, A]] =
    t match {
      case Leaf(shape, _) => Rose(shape)
      case Graft(cell, branches, _) => Branch(cell, branches map (b => toRoseTree(b, hasPred)))
    }

  def fromRoseTree[D <: Nat : IsZero, A](t : RoseTree[Cell[D, A], Unit]) : CellTree[D, A] =
    t match {
      case Branch(obj, Vector(Rose(()))) => Seed(obj).asInstanceOf[CellTree[D, A]]
      case _ => throw new IllegalArgumentException("Malformed object tree")
    }

  def fromRoseTree[D <: Nat : HasPred, A](t : RoseTree[Cell[D, A], Cell[D#Pred, A]]) : CellTree[D, A] = 
    t match {
      case Rose(shape) => Leaf(shape)
      case Branch(cell, branches) => Graft(cell, branches map (b => fromRoseTree(b)))
    }


  /**
    * Operations on CellTrees
    * 
    */

  implicit class CellTreeOps[D <: Nat, A](tree : CellTree[D, A]) {

    def dimension : D = 
      tree match {
        case Seed(obj, _) => _0.asInstanceOf[D]
        case Leaf(shape, _) => S(shape.dimension).asInstanceOf[D]
        case Graft(cell, branches, _) => cell.dimension
      }

    def isLeaf : Boolean = 
      tree match {
        case Graft(_, _, _) => false
        case _ => true
      }

    def foreach(action : Cell[D, A] => Unit) : Unit =
      tree match {
        case Seed(obj, _) => action(obj)
        case Leaf(shape, _) => ()
        case Graft(cell, branches, _) => {
          branches foreach (branch => branch foreach action)
          action(cell)
        }
      }

    def cells : Vector[Cell[D, A]] =
      tree match {
        case Seed(obj, _) => Vector(obj)
        case Leaf(shape, _) => Vector.empty
        case Graft(cell, branches, _) => (branches flatMap (_.cells)) :+ cell
      }

    def zip[B](other : CellTree[D, B]) : Option[CellTree[D, (A, B)]] = 
      tree match {
        case Seed(to, ev) => {
          implicit val isZero = ev
          other match {
            case Seed(oo, _) => for { res <- to.zip(oo) } yield SeedClass(res.asInstanceOf[ObjectCell[D, (A, B)]])
            case _ => None
          }
        }
        case Leaf(ts, ev) => {
          implicit val hasPred = ev
          other match {
            case Leaf(os, _) => for { res <- ts.zip(os) } yield Leaf(res)
            case _ => None
          }
        }
        case Graft(tc, tbs, ev) => {
          implicit val hasPred = ev
          other match {
            case Graft(oc, obs, _) => {
              for { zcell <- tc.zip(oc) 
                    zbrs <- optSwitchVect(tbs.zip(obs) map 
                      (pr => { val (tb, ob) = pr ; tb.zip(ob) })) } 
              yield Graft(zcell, zbrs)
            }
            case _ => None
          }
        }
      }

    def leaves : Vector[Cell[D#Pred, A]] = 
      tree match {
        case Seed(_, _) => Vector.empty
        case Leaf(shape, _) => Vector(shape)
        case Graft(_, branches, _) => branches flatMap (_.leaves)
      }

    def target[B >: A](tgtValue : B) : Cell[D, B] =
      tree match {
        case Seed(obj, ev) => {
          implicit val isZero = ev
          ObjectCell(tgtValue)
        }
        case Leaf(shape, ev) => {
          implicit val hasPred = ev
          CompositeCell(tgtValue, shape.corolla, shape.value)
        }
        case Graft(cell, branches, ev) => {
          implicit val hasPred = ev
          CompositeCell(tgtValue, tree.flatten, cell.targetValue)
        }
      }

    def output(implicit hasPred : HasPred[D]) : Cell[D#Pred, A] =
      tree match {
        case Leaf(shape, _) => shape
        case Graft(cell, branches, _) => cell.target
      }

    def outputOption : Option[Cell[D#Pred, A]] = 
      tree match {
        case Seed(_, _) => None
        case Leaf(shape, _) => Some(shape)
        case Graft(cell, branches, ev) => {
          implicit val hasPred = ev
          Some(cell.target)
        }
      }

    def seek(loc : List[Int]) : Option[CellTree[D, A]] = 
      loc match {
        case Nil => Some(tree)
        case i :: is => 
          tree match {
            case Seed(_, _) => None
            case Leaf(_, _) => None
            case Graft(_, branches, _) => {
              if (branches.isDefinedAt(i)) {
                branches(i).seek(is)
              } else None
            }
          }
      }

    def getUnit : CellTree[D, A] =
      tree match {
        case Graft(cell, branches, ev) => {
            implicit val hasPred : HasPred[D] = ev
            LeafClass(cell.target)
          }
        case other => other
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

    def permutationInfo(implicit hasPred : HasPred[D]) : RoseTree[Cell[D, A], (Cell[D#Pred, A], Int)] = {

      var counter : Int = -1

      def buildPermTree(t : CellTree[D, A]) : RoseTree[Cell[D, A], (Cell[D#Pred, A], Int)] = 
        t match {
          case Leaf(shape, _) => {
            counter += 1
            Rose(shape, counter)
          }
          case Graft(cell, branches, _) => Branch(cell, branches map buildPermTree)
        }

      buildPermTree(tree)
    }

    def inversePerm : Vector[Int] = invertPerm(permutation)

    def permutation : Vector[Int] = 
      dimension match {
        case IsZero(_) => Vector.empty
        case HasPred(ev) => {

          implicit val hasPred : HasPred[D] = ev

          def flattenPermTree(pt : RoseTree[Cell[D, A], (Cell[D#Pred, A], Int)], lvs : Vector[RoseTree[Int, Unit]]) : RoseTree[Int, Unit] =
            pt match {
              case Rose((shape, idx)) =>
                shape match {
                  case Object(_, _) => Branch(idx, Vector(Rose(()))) // Ummm .....
                  case Composite(_, srcTree, _, _) => Branch(idx, lvs)
                }
              case Branch(guideCell, verticalBranches) => {

                val thePerm : Vector[Int] =
                  guideCell.dimension match {
                    case IsZero(_) => Vector.empty
                    case HasPred(ev) => {
                      implicit val hasPred : HasPred[D] = ev
                      guideCell.srcTree.inversePerm
                    }
                  }

                var counter : Int = -1
                val verticalStack = new Stack() ++ verticalBranches

                def horizontalTrace(t : CellTree[D#Pred, A]) : RoseTree[Int, Unit] = {
                  t match {
                    case Seed(_, _) => {
                      counter += 1
                      flattenPermTree(verticalStack.pop, lvs)
                    }
                    case Leaf(_, _) => {
                      counter += 1
                      lvs(thePerm(counter))
                    }
                    case Graft(connectingCell, horizontalBranches, _) => {
                      val newBranches = horizontalBranches map horizontalTrace
                      flattenPermTree(verticalStack.pop, newBranches)
                    }
                  }
                }

                horizontalTrace(guideCell.srcTree)
              }
            }

          val startLeaves =
            tree match {
              case Leaf(shape, _) =>
                shape match {
                  case Object(_, _) => Vector.empty
                  case Composite(_, srcTree, _, _) => srcTree.cells map (_ => Rose(()))
                }
              case Graft(cell, _, _) => cell.srcTree.leaves map (_ => Rose(()))
            }

          flattenPermTree(tree.permutationInfo, startLeaves).nodeVector
        }
      }


    def flatten(implicit hasPred : HasPred[D]) : CellTree[D#Pred, A] = 
      tree match {
        case Leaf(shape, _) => shape.corolla
        case Graft(cell, branches, _) => {
          val startLeaves : Vector[CellTree[D#Pred, A]] = cell.srcTree.dimension match {
            case IsZero(_) => Vector.empty
            case HasPred(ev) => {
              implicit val hasPredPred : HasPred[D#Pred] = ev
              cell.srcTree.flatten.cells map (l => Leaf(l))
            }
          }

          tree.flattenLocal(startLeaves)
        }
      }

    def flattenLocal[B >: A](lvs : Vector[CellTree[D#Pred, B]])(implicit hasPred : HasPred[D]) : CellTree[D#Pred, B] =
      tree match {
        case Leaf(shape, _) => {
          shape match {
            case Object(value, ev) => {
              // Uh.  Not sure ....
              implicit val isZero = ev
              SeedClass(ObjectCell(value))
            }
            case Composite(value, srcTree, tgtValue, ev) => {
              implicit val hasPredPred = ev
              GraftClass(shape, lvs)
            }
          }
        }

        case Graft(guideCell, verticalBranches, _) => {

          val thePerm : Vector[Int] =
            guideCell.dimension match {
              case IsZero(_) => Vector.empty
              case HasPred(ev) => {
                implicit val hasPred : HasPred[D] = ev
                guideCell.srcTree.inversePerm
              }
            }

          var counter : Int = -1
          val verticalStack = new Stack ++ verticalBranches

          def horizontalTrace(t : CellTree[D#Pred, B]) : CellTree[D#Pred, B] = 
            t match {
              case Seed(_, _) => {
                counter += 1
                verticalStack.pop.flattenLocal(lvs)
              }
              case Leaf(s, _) => {
                counter += 1
                lvs(thePerm(counter))
              }
              case Graft(c, hbrs, _) => {
                val newBranches = hbrs map horizontalTrace
                verticalStack.pop.flattenLocal(newBranches)
              }
            }

          horizontalTrace(guideCell.srcTree)
        }
      }

    def regenerateFrom[T >: A, B](generator : CellRegenerator[T, B], 
                                  canopy : CellTree[D#Pred, B])(implicit hasPred : HasPred[D])
        : CellTree[D, B] = {

      val vLvs = canopy.cells
      val hLvs : Vector[CellTree[D#Pred, B]] =
        canopy.dimension match {
          case IsZero(_) => Vector.empty
          case HasPred(ev) => {
            implicit val hasPredPred : HasPred[D#Pred] = ev
            canopy.flatten.cells map (l => Leaf(l))
          }
        }

      var vCount = -1
      val vPerm = inversePerm

      def verticalTrace(t : CellTree[D, A], lvs : Vector[CellTree[D#Pred, B]]) : CellTree[D, B] =
        t match {
          case Leaf(_, ev) => {
            vCount += 1
            Leaf(vLvs(vPerm(vCount)))
          }
          case Graft(guideCell, verticalBranches, ev) => {

            val verticalStack = new Stack ++ verticalBranches
            val finishedVertical = new ListBuffer[CellTree[D, B]]

            var hCount = -1
            val hPerm = guideCell.srcTree.inversePerm

            def horizontalTrace(tr : CellTree[D#Pred, A]) : CellTree[D#Pred, B] =
              tr match {
                case Seed(o, e) => {
                  implicit val isZero : IsZero[D#Pred] = e
                  hCount += 1

                  val thisBranch = verticalTrace(verticalStack.pop, lvs)
                  finishedVertical += thisBranch

                  Seed(thisBranch.output).asInstanceOf[CellTree[D#Pred, B]]
                }
                case Leaf(_, _) => {
                  hCount += 1
                  lvs(hPerm(hCount))
                }
                case Graft(_, horizontalBranches, e) => {
                  implicit val hasPredPred = e

                  // First pass to the horizontalBranches
                  val newBranches = horizontalBranches map horizontalTrace
                  val newLeaves = newBranches map (b => Leaf(b.output))

                  val thisBranch = verticalTrace(verticalStack.pop, newLeaves)
                  finishedVertical += thisBranch

                  Graft(thisBranch.output, newBranches)
                }
              }

            val newSrcTree = horizontalTrace(guideCell.srcTree)
            val newCell = generator.generateCell(guideCell.value, newSrcTree, guideCell.targetValue)

            Graft(newCell, finishedVertical.toVector)
          }
        }

      verticalTrace(tree, hLvs)
    }
  }

  // Some higher dimensional number stuff which may be useful some time ...

  // def hdn : RoseTree[HDN[D], Unit] =
  //   dimension match {
  //     case Z => Branch(HZero.asInstanceOf[HDN[D]], List(Rose()))
  //     case S(p) => hdn(None, HDN(dimension.asInstanceOf[D]))
  //   }

  // def hdn(root : Option[HDN[D#Pred]], path : HDN[D]) : RoseTree[HDN[D], Unit] =
  //   tree match {
  //     case Seed(_, _) => Branch(HZero.asInstanceOf[HDN[D]], List(Rose()))
  //     case Leaf(_, _) => Rose(())
  //     case Graft(cell, branches, ev) => {
  //       implicit val hasPred = ev

  //       // Horrible.
  //       val hdVal : HDN[D] =
  //         root match {
  //           case None => path
  //           case Some(r) => (r +: path.asInstanceOf[HDN[S[D#Pred]]]).asInstanceOf[HDN[D]]
  //         }

  //       val valBranches = (cell.srcTree.hdn.toList zip branches) map (pr => {
  //         val (hr, br) = pr
  //         br.hdn(Some(hr), hdVal)
  //       })

  //       Branch(hdVal, valBranches)
  //     }
  //   }

}
