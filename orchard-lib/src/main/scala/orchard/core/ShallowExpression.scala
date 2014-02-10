/**
  * ShallowExpression.scala - A shallow implementation of expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import Nats._

case class ShallowNook(val srcs : RoseTree[Option[String], Option[String]], val tgt : Option[String])

sealed trait ShallowExpression { def id : String ; def isThin : Boolean }

case class ShallowVar(val id : String, val isThin : Boolean) extends ShallowExpression { 
  override def toString = id 
}

case class ShallowFace(val toks : List[IdentToken], val nook : ShallowNook, val isThin : Boolean) extends ShallowExpression {
  override def toString = id

  def id : String = IdentToken.getId(toks)
}

case class ShallowFiller(val toks : List[IdentToken], val nook : ShallowNook) extends ShallowExpression { 
  override def toString = id 

  def isThin = true 
  def id : String = IdentToken.getId(toks)
}

object ShallowExpression {

  def apply(expr : Expression) : ShallowExpression = 
    expr match {
      case Variable(id, isThin) => ShallowVar(id, isThin)
      case FillerFace(toks, nook, isThin) => {
        // Hmm.  But what about the further nestings?  Do they matter?
        ShallowFace(toks, new SimpleFramework(nook).getShallowNook, isThin)
      }
      case Filler(toks, nook) => {
        ShallowFiller(toks, new SimpleFramework(nook).getShallowNook)
      }
    }

  def decodeNook(nook : ShallowNook, dim : Nat, srcCell : Option[NCell[Expression]]) : NCell[Option[Expression]] = 
    nook match {
      case ShallowNook(srcs, tgt) => {
        val test = srcs map (
          (node => node match {
            case None => {
              // If this is the case, we need to know the shape of the cell
              // in question. 
            }
            case Some(srcId) => ???
          }),
          (leaf => ???)
        )

        ???
      }
    }

  def toExpression(expr : ShallowExpression, 
                   shallowEnv : Seq[NCell[ShallowExpression]],
                   deepEnv : Map[String, NCell[Expression]]) : Expression = {

    def getFromEnvironment(id : String) : NCell[ShallowExpression] = 
      (shallowEnv find (expr => expr.value.id == id)).get

    def getOrRebuild(id : String) : NCell[Expression] = {
      if (! deepEnv.isDefinedAt(id)) {
        deepEnv(id) = 
          shallowEnv find (expr => expr.value.id == id) match {
            case None => throw new IllegalArgumentException("Could not find shape information for " ++ id)
            case Some(shallowExpr) => {
              shallowExpr map (se => toExpression(se, shallowEnv, deepEnv))
            }
          }
      } 

      deepEnv(id)
    }

    expr match {
      case ShallowVar(id, isThin) => Variable(id, isThin)
      case ShallowFace(toks, nook, isThin) => {
        // val myCell = getFromEnvironment(IdentToken.getId(toks))

        // myCell.dim match {
        //   case HasPred(e) => {
        //     implicit val hasPred = e

        //     getPred(myCell.dim) match {
        //       case IsZero(_) => {
        //         nook.tgt match {
        //           case None => ???
        //           case Some(tgtId) => ???
        //         }
        //       }
        //       case HasPred(ev) => {
        //         implicit val hasPredPred : HasPred[myCell.Dim#Pred] = ev

        //         nook.tgt match {
        //           case None => {
        //             val nookSrcTree =
        //               CellTree.fromRoseTree[myCell.Dim#Pred, Option[Expression]](
        //                 nook.srcs.map(
        //                   (node => (getFromEnvironment(node.get) map (s => Some(toExpression(s, env)))).
        //                     asInstanceOf[Cell[myCell.Dim#Pred, Option[Expression]]]),
        //                   (leaf => (getFromEnvironment(leaf.get) map (s => Some(toExpression(s, env)))).
        //                     asInstanceOf[Cell[myCell.Dim#Pred#Pred, Option[Expression]]])
        //                 ))

        //             FillerFace(toks, Composite(None, nookSrcTree, None), isThin)
        //           }

        //           case Some(tgtId) => {
        //             val nookSrcTree = 
        //               CellTree.fromRoseTree[myCell.Dim#Pred, Option[Expression]](
        //                 nook.srcs.map(
        //                   (node => (getFromEnvironment(node.get) map (s => Some(toExpression(s, env)))).
        //                     asInstanceOf[Cell[myCell.Dim#Pred, Option[Expression]]]),
        //                   (leaf => (getFromEnvironment(leaf.get) map (s => Some(toExpression(s, env)))).
        //                     asInstanceOf[Cell[myCell.Dim#Pred#Pred, Option[Expression]]])
        //                 ))
        //           }
        //         }
        //       }
        //     }
        //   }
        // }

        ???
      }
      case ShallowFiller(toks, nook) => {
        ???
      }
    }
  }

  def toExpression(expr : ShallowExpression, env : Seq[NCell[ShallowExpression]]) : Expression = {
    // Okay, here is where the work is.  We need to use the environment to reassemble the pieces

    def getFromEnvironment(id : String) : NCell[ShallowExpression] = 
      (env find (expr => expr.value.id == id)).get

    def rebuildFromEnvironment(id : String) : NCell[Option[Expression]] = 
      (getFromEnvironment(id) map (s => Some(toExpression(s, env)))).
        asInstanceOf[Cell[_ <: Nat, Option[Expression]]]

    expr match {
      case ShallowVar(id, isThin) => Variable(id, isThin)
      case ShallowFace(toks, nook, isThin) => {
        val myCell = getFromEnvironment(IdentToken.getId(toks))

        myCell.dim match {
          case HasPred(e) => {
            implicit val hasPred = e

            getPred(myCell.dim) match {
              case IsZero(_) => {
                nook.tgt match {
                  case None => ???
                  case Some(tgtId) => ???
                }
              }
              case HasPred(ev) => {
                implicit val hasPredPred : HasPred[myCell.Dim#Pred] = ev

                nook.tgt match {
                  case None => {
                    val nookSrcTree =
                      CellTree.fromRoseTree[myCell.Dim#Pred, Option[Expression]](
                        nook.srcs.map(
                          (node => (getFromEnvironment(node.get) map (s => Some(toExpression(s, env)))).
                            asInstanceOf[Cell[myCell.Dim#Pred, Option[Expression]]]),
                          (leaf => (getFromEnvironment(leaf.get) map (s => Some(toExpression(s, env)))).
                            asInstanceOf[Cell[myCell.Dim#Pred#Pred, Option[Expression]]])
                        ))

                    FillerFace(toks, Composite(None, nookSrcTree, None), isThin)
                  }

                  case Some(tgtId) => {
                    val nookSrcTree = 
                      CellTree.fromRoseTree[myCell.Dim#Pred, Option[Expression]](
                        nook.srcs.map(
                          (node => (getFromEnvironment(node.get) map (s => Some(toExpression(s, env)))).
                            asInstanceOf[Cell[myCell.Dim#Pred, Option[Expression]]]),
                          (leaf => (getFromEnvironment(leaf.get) map (s => Some(toExpression(s, env)))).
                            asInstanceOf[Cell[myCell.Dim#Pred#Pred, Option[Expression]]])
                        ))
                  }
                }
              }
            }
          }
        }
      }
      case ShallowFiller(toks, nook) => {
      }

    }

    ???
  }
}
