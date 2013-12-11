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
import Util._

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
                    zbrs <- optSwitch(tbs.zip(obs) map 
                      (pr => { val (tb, ob) = pr ; tb.zip(ob) })) } 
              yield Graft(zcell, zbrs)
            }
            case _ => None
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

    // def substitute[B >: A](trees : List[CellTree[S[D], B]]) : CellTree[D, B] =
    //   tree match {
    //     case Seed(obj, _) => trees.head.leaves
    //     case Leaf(shape, ev) => 
    //       {
    //         implicit val hasPred = ev
    //         LeafClass(shape)
    //       }
    //     case graft => graft.substituteLocal(trees)._1
    //   }

    // def substituteLocal[B >: A](trees : List[CellTree[S[D], B]])
    //     : (CellTree[D, B], List[CellTree[S[D] , B]]) =
    //   tree match {
    //     case Seed(obj, _) => (trees.head.leaves, trees.tail)
    //     case Leaf(shape, ev) => 
    //       {
    //         implicit val hasPred = ev
    //         (LeafClass(shape), trees)
    //       }
    //     case Graft(cell, branches, ev) => 
    //       {
    //         implicit val hasPred = ev

    //         def consume(brs : List[CellTree[D, A]],
    //                     trs : List[CellTree[S[D], B]])
    //             : (List[CellTree[D, B]], List[CellTree[S[D], B]]) = brs match {
    //             case Nil => (Nil, trs)
    //             case b :: bs => {
    //               val (t, rem0) = b.substituteLocal(trs)
    //               val (ts, rem1) = consume(bs, rem0)
    //               (t :: ts, rem1)
    //             }
    //           }

    //         val (newBranches, rem) = consume(branches, trees)
    //         val newNode = rem.head.leaves

    //         (newNode.graft(newBranches), rem.tail)
    //       }
    //   }

    // Okay, this is wrong, but I see how to fix it.  What you can do is actually build the
    // tree by passing partials backwards as in the attempt at substitution below.  This will
    // build a RoseTree[Int, ()].  Then spit out the list of cells and voila! you've got your
    // permutation.

    // It's a little more subtle than this, but I think I see it.  Here's the point: as your 
    // last experiment showed, this method does not automatically produce the correct flattened
    // tree.  The reason is that the horizontal trace *also* needs to incorporate the permuation
    // at that level.  But now you see the solution: since you are writing the permutation 
    // function, you can call if (by induction, if you like) before you start to do the horizontal
    // trace and then use this as a lookup when you get to the leaves.

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

    // To me, this looks right.  It in fact passes uses all the lower dimensional information
    // to hand off the leaves according to the permutation.  Let's see if we can use it to flatten.

    def invertPerm(p : List[Int]) : List[Int] = {
      val invP = Range(0, p.length).toList map (i => (p(i), i))
      invP.toList.sorted map (pr => { val (_, v) = pr ; v })
    }

    def inversePerm : List[Int] = invertPerm(permutation)

    def permutation : List[Int] = 
      dimension match {
        case IsZero(_) => Nil
        case HasPred(ev) => {

          implicit val hasPred : HasPred[D] = ev

          def flattenPermTree(pt : RoseTree[Cell[D, A], (Cell[D#Pred, A], Int)], lvs : List[RoseTree[Int, Unit]]) : RoseTree[Int, Unit] =
            pt match {
              case Rose((shape, idx)) =>
                // I think the idea is that at a rose, the permutation is trivial (this is the
                // assumption that external cells use the correct ordering.)
                shape match {
                  case Object(_, _) => Branch(idx, List(Rose(()))) // Ummm .....
                  case Composite(_, srcTree, _, _) => Branch(idx, lvs)
                }
              case Branch(guideCell, verticalBranches) => {

                val thePerm : List[Int] =
                  guideCell.dimension match {
                    case IsZero(_) => Nil
                    case HasPred(ev) => {
                      implicit val hasPred : HasPred[D] = ev
                      guideCell.srcTree.inversePerm
                    }
                  }

                var counter : Int = -1

                // We want to traverse the guide cell, calling this flatten function as we go on the vertical
                // branches with the correct incoming leaves.

                def horizontalTraverse(t : CellTree[D#Pred, A], vBrs : List[RoseTree[Cell[D, A], (Cell[D#Pred, A], Int)]])
                    : (RoseTree[Int, Unit], List[RoseTree[Cell[D, A], (Cell[D#Pred, A], Int)]]) = {

                  t match {
                    case Seed(_, _) => {
                      // This means we are on a 1-d tree.  So there should be exactly one vertical branch, which
                      // we should follow until we get to a rose.  Moreover, I think the horizontal leaf list should
                      // be empty, though I won't bother to check.
                      counter += 1
                      (flattenPermTree(vBrs.head, lvs), vBrs.tail)
                    }
                    case Leaf(_, _) => {
                      // I think we want to grab things off the list by index, and then drop the
                      // total that we have used at the end.  So we don't need to keep the horizontal
                      // leaves as a state variable.
                      counter += 1
                      (lvs(thePerm(counter)), vBrs)
                    }

                    case Graft(connectingCell, horizontalBranches, _) => {

                      def processHorizBranches(brs : List[CellTree[D#Pred, A]], vbrs : List[RoseTree[Cell[D, A], (Cell[D#Pred, A], Int)]])
                          : (List[RoseTree[Int, Unit]], List[RoseTree[Cell[D, A], (Cell[D#Pred, A], Int)]]) =
                        brs match {
                          case Nil => (Nil, vbrs)
                          case b :: bs => {
                            val (thisBranch, vertsAfterBranch) = horizontalTraverse(b, vbrs)
                            val (theseBranches, remVerts) = processHorizBranches(bs, vertsAfterBranch)
                            (thisBranch :: theseBranches, remVerts)
                          }
                        }

                      val (newBranches, remVert) = processHorizBranches(horizontalBranches, vBrs)
                      val theResult = flattenPermTree(remVert.head, newBranches)

                      (theResult, remVert.tail)
                    }
                  }
                }

                val (theResultTree, vertShouldBeNil) =
                  horizontalTraverse(guideCell.srcTree, verticalBranches)

                assert(vertShouldBeNil == Nil)

                theResultTree 
              }
            }

          val startLeaves =
            tree match {
              case Leaf(shape, _) =>
                shape match {
                  case Object(_, _) => Nil
                  case Composite(_, srcTree, _, _) => srcTree.cellList map (_ => Rose(()))
                }
              case Graft(cell, _, _) => cell.srcTree.leafList map (_ => Rose(()))
            }

          // The input here is a bit of a question ....
          flattenPermTree(tree.permutationInfo, startLeaves).toList
        }
      }

    // SUCCESS!!! It finally works.  There are some massive, massive inefficiencies,
    // but at least you got the gist of it.

    def flatten(implicit hasPred : HasPred[D]) : CellTree[D#Pred, A] = 
      tree match {
        case Leaf(shape, _) => shape.corolla
        case Graft(cell, branches, _) => {
          val startLeaves : List[CellTree[D#Pred, A]] = cell.srcTree.dimension match {
            case IsZero(_) => Nil
            case HasPred(ev) => {
              implicit val hasPredPred : HasPred[D#Pred] = ev
              cell.srcTree.flatten.cellList map (l => Leaf(l))
            }
          }

          tree.flattenLocal(startLeaves)
        }
      }

    def flattenLocal[B >: A](lvs : List[CellTree[D#Pred, B]])(implicit hasPred : HasPred[D]) : CellTree[D#Pred, B] =
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

          val thePerm : List[Int] =
            guideCell.dimension match {
              case IsZero(_) => Nil
              case HasPred(ev) => {
                implicit val hasPred : HasPred[D] = ev
                guideCell.srcTree.inversePerm
              }
            }

          var counter : Int = -1

          def horizontalTrace(t : CellTree[D#Pred, B], vTrs : List[CellTree[D, B]]) 
              : (CellTree[D#Pred, B], List[CellTree[D, B]]) = 
            t match {
              case Seed(_, _) => {
                counter += 1
                (vTrs.head.flattenLocal(lvs), vTrs.tail)
              }
              case Leaf(s, _) => {
                counter += 1
                (lvs(thePerm(counter)), vTrs)
              }
              case Graft(c, hbrs, _) => {

                def doBranches(brs : List[CellTree[D#Pred, B]], vtrs : List[CellTree[D, B]]) 
                    : (List[CellTree[D#Pred, B]], List[CellTree[D, B]]) =
                  brs match {
                    case Nil => (Nil, vtrs)
                    case b :: bs => {
                      val (thisBranch, remVertAfterBranch) = horizontalTrace(b, vtrs)
                      val (remBranches, remVert) = doBranches(bs, remVertAfterBranch)
                      (thisBranch :: remBranches, remVert)
                    }
                  }

                val (theBranches, theVerticals) = doBranches(hbrs, vTrs) 
                (theVerticals.head.flattenLocal(theBranches), theVerticals.tail)
              }
            }

          val (theResultTree, vertShouldBeNil) = horizontalTrace(guideCell.srcTree, verticalBranches)

          assert(vertShouldBeNil == Nil)

          theResultTree
        }
      }

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

    def leafList : List[Cell[D#Pred, A]] =
      tree match {
        case Seed(_, _) => Nil
        case Leaf(shape, _) => shape :: Nil
        case Graft(cell, branches, _) => branches flatMap (_.leafList)
      }

    def lvs : List[CellTree[D, A]] = 
      tree match {
        case Seed(_, _) => Nil
        case Graft(_, branches, _) => branches flatMap (_.lvs)
        case l => List(l)
      }

    // This is a shitty name for this idea ...
    // How about "canopy"?  You use that in a kind
    // of similar sense in the imperative version ...
    // def leaves(implicit hasPred : HasPred[D]) : CellTree[D#Pred, A] = 
    //   tree match {
    //     case Leaf(shape, _) => shape.corolla
    //     case Graft(cell, branches, _) =>
    //       cell.srcTree.substitute(branches)
    //   }

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
            CompositeCell(tgtValue, shape.corolla, shape.value)
          }
        case Graft(cell, branches, ev) =>
          {
            implicit val hasPred = ev
            CompositeCell(tgtValue, tree.flatten, cell.targetValue)
          }
      }

    def output(implicit hasPred : HasPred[D]) : Cell[D#Pred, A] =
      tree match {
        case Leaf(shape, _) => shape
        case Graft(cell, branches, _) => cell.target
      }

    def regenerateFrom[T >: A, B](generator : CellRegenerator[T, B], 
                                  canopy : CellTree[D#Pred, B])(implicit hasPred : HasPred[D])
        : CellTree[D, B] = {


      val vLvs = canopy.cellList
      val hLvs : List[CellTree[D#Pred, B]] =
        canopy.dimension match {
          case IsZero(_) => Nil
          case HasPred(ev) => {
            implicit val hasPredPred : HasPred[D#Pred] = ev
            canopy.flatten.cellList map (l => Leaf(l))
          }
        }

      var vCount = -1
      val vPerm = inversePerm

      def verticalTrace(t : CellTree[D, A], lvs : List[CellTree[D#Pred, B]]) : CellTree[D, B] =
        t match {
          case Leaf(_, ev) => {
            vCount += 1
            Leaf(vLvs(vPerm(vCount)))
          }
          case Graft(guideCell, verticalBranches, ev) => {

            var remainingVertical = verticalBranches
            val finishedVertical = new ListBuffer[CellTree[D, B]]

            var hCount = -1
            val hPerm = guideCell.srcTree.inversePerm

            def horizontalTrace(tr : CellTree[D#Pred, A]) : CellTree[D#Pred, B] =
              tr match {
                case Seed(o, e) => {
                  implicit val isZero : IsZero[D#Pred] = e
                  hCount += 1

                  // This is probably better done with a ListBuffer as well ...
                  val thisBranch = verticalTrace(remainingVertical.head, lvs)
                  remainingVertical = remainingVertical.tail

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

                  val thisBranch = verticalTrace(remainingVertical.head, newLeaves)
                  remainingVertical = remainingVertical.tail

                  finishedVertical += thisBranch

                  Graft(thisBranch.output, newBranches)
                }
              }

            val newSrcTree = horizontalTrace(guideCell.srcTree)
            val newCell = generator.generateCell(guideCell.value, newSrcTree, guideCell.targetValue)

            Graft(newCell, finishedVertical.toList)
          }
        }

      verticalTrace(tree, hLvs)
    }
  }
}
