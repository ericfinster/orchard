/**
  * Cell.scala - Opetopic Cells
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core.cell

import scala.language.implicitConversions
import scala.collection.mutable.Stack

import orchard.core.util._
import Nats._

sealed trait Cell[D <: Nat, +A]

case class ObjectCell[D <: Nat, +A](value : A)(implicit val isZero : IsZero[D]) extends Cell[D, A] {
  override def toString = value.toString
}

case class CompositeCell[D <: Nat, +A](value : A, srcTree : CellTree[D#Pred, A], tgtValue : A)(implicit val hasPred : HasPred[D]) extends Cell[D, A] {
  override def toString = value.toString // ++ " : " ++ (srcTree.cells map (cell => cell.value.toString)).toString ++ " -> " ++ tgtValue.toString
}

object Object {
  def apply[A](value : A) : ObjectCell[_0, A] = ObjectCell(value)

  def unapply[D <: Nat, A](cell : Cell[D, A]) : Option[(A, IsZero[D])] =
    if (cell.isInstanceOf[ObjectCell[D, A]]) {
      val c = cell.asInstanceOf[ObjectCell[D, A]]
      Some(c.value, c.isZero)
    } else {
      None
    }
}

object Composite {
  def apply[D <: Nat, A](value : A, srcTree : CellTree[D, A], tgtValue : A) : CompositeCell[S[D], A] =
    CompositeCell[S[D], A](value, srcTree, tgtValue)

  def unapply[D <: Nat, A](cell : Cell[D, A]) : Option[(A, CellTree[D#Pred, A], A, HasPred[D])] =
    if (cell.isInstanceOf[CompositeCell[D, A]]) {
      val c = cell.asInstanceOf[CompositeCell[D, A]]
      Some(c.value, c.srcTree, c.tgtValue, c.hasPred)
    } else {
      None
    }
}

object Cell {

  //============================================================================================
  // COERCIONS
  //

  implicit def toSucc[D <: Nat : HasPred, A](tree : Cell[D, A]) : Cell[S[D#Pred], A] =
      tree.asInstanceOf[Cell[S[D#Pred], A]]

  implicit def toSuccVect[D <: Nat : HasPred, A](l : Vector[Cell[D, A]]) : Vector[Cell[S[D#Pred], A]] =
      l map (t => toSucc(t)(implicitly[HasPred[D]]))

  implicit def fromSucc[D <: Nat : HasPred, A](tree : Cell[S[D#Pred], A]) : Cell[D, A] =
      tree.asInstanceOf[Cell[D, A]]

  implicit def fromSuccVect[D <: Nat : HasPred, A](l : Vector[Cell[S[D#Pred], A]]) : Vector[Cell[D, A]] =
      l map (t => fromSucc(t)(implicitly[HasPred[D]]))

  //============================================================================================
  // CELL OPERATIONS
  //

  implicit class CellOps[D <: Nat, A](cell : Cell[D, A]) {

    def glob[B >: A](tgtLbl : B, globLbl : B) : Cell[S[D], B] =
      CompositeCell[S[D], B](globLbl, cell.corolla, tgtLbl)

    def drop[B >: A](globLbl : B, dropLbl : B) : Cell[S[S[D]], B] =
      CompositeCell[S[S[D]], B](dropLbl, LeafClass[S[D], B](cell), globLbl)

    def map[B](f : A => B) : Cell[D, B] = cell.regenerateFrom(CellRegenerator.mapRegenerator(f))

    def zip[B](other : Cell[D, B]) : Option[Cell[D, (A, B)]] = 
      cell match {
        case Object(cv, ev) => {
          implicit val isZero = ev
          other match {
            case Object(ov, _) => Some(Object((cv, ov)).asInstanceOf[Cell[D, (A, B)]])
            case _ => None
          }
        }
        case Composite(cv, cst, ctv, ev) => {
          implicit val hasPred = ev
          other match {
            case Composite(ov, ost, otv, _) => {
              for { res <- cst.zip(ost) } yield Composite((cv, ov), res, (ctv, otv))
            }
            case _ => None
          }
        }
      }

    def comultiply : Cell[D, NCell[A]] = 
      cell match {
        case Object(value, ev) => {
          implicit val isZero = ev
          ObjectCell(cell)
        }
        case Composite(value, srcTree, tgtValue, ev) => {
          implicit val hasPred = ev
          CompositeCell(cell, srcTree.comultiply, cell.target)
        }
      }

    def finalObject : Cell[_0, A] =
      cell match {
        case Object(value, _) => Object(value)
        case Composite(_, _, _, ev) => {
          implicit val hasPred = ev
          cell.target.finalObject
        }
      }

    def nthTarget(n : Int) : Cell[_ <: Nat, A] = {
      if (n <= 0) cell else {
        if (cell.isInstanceOf[CompositeCell[D, A]]) {
          val compCell = cell.asInstanceOf[CompositeCell[D, A]]
          implicit val hasPred = compCell.hasPred
          compCell.target.nthTarget(n - 1)
        } else {
          throw new IllegalArgumentException("Object has no target.")
        }
      }
    }

    def targetValues : List[A] =
      cell match {
        case Object(value, _) => value :: Nil
        case Composite(value, _, _, ev) => {
          implicit val hasPred = ev
          value :: cell.target.targetValues
        }
      }

    def dimension : D =
      cell match {
        case Object(value, _) => _0.asInstanceOf[D]
        case Composite(_, srcTree, _, _) => S(srcTree.dimension).asInstanceOf[D]
      }

    def value : A =
      cell match {
        case Object(value, _) => value
        case Composite(value, _, _, _) => value
      }

    def srcTree(implicit hasPred : HasPred[D]) : CellTree[D#Pred, A] =
      cell match {
        case Composite(_, srcTree, _, _) => srcTree
      }

    def targetValue(implicit hasPred : HasPred[D]) : A =
      cell match {
        case Composite(_, _, tgtValue, _) => tgtValue
      }

    def corolla : CellTree[D, A] =
      cell match {
        case Object(value, ev) => {
          implicit val isZero = ev
          SeedClass(ObjectCell(value))
        }
        case Composite(value, srcTree, tgtValue, ev) => {
          implicit val hasPred = ev
          GraftClass(CompositeCell(value, srcTree, tgtValue),
            srcTree.cells.map(s => LeafClass(s)))
        }
      }

    def sources : Vector[Cell[D#Pred, A]] =
      cell match {
        case Object(_, _) => Vector.empty
        case Composite(_, srcTree, _, _) => srcTree.cells
      }

    def target(implicit hasPred : HasPred[D]) : Cell[D#Pred, A] = srcTree.target(targetValue)

    def regenerateFrom[T >: A, B](generator : CellRegenerator[T, B]) : Cell[D, B] =
      cell match {
        case Object(value, ev) => {
            implicit val isZero : IsZero[D] = ev
            generator.generateObject(value)
          }
        case Composite(value, srcTree, tgtValue, ev) => {
            implicit val hasPred : HasPred[D] = ev
            cell.target.regenerateFromCtxt(generator, cell.corolla).cells.head
          }
      }

    def regenerateFromCtxt[T >: A, B](generator : CellRegenerator[T, B], ctxt : CellTree[S[D], T])
        : CellTree[S[D], B] = 
      cell match {
        case Object(_, ev) => {
            val obj = ctxt.leaves.head
            val result = obj.regenerateFrom(generator)
            ctxt.regenerateFrom(generator, result.corolla)
          }
        case Composite(_, _, _, ev) => {
            implicit val hasPred : HasPred[D] = ev
            val lvs = cell.target.regenerateFromCtxt(generator, ctxt.flatten)
            ctxt.regenerateFrom(generator, lvs)
          }
      }
  }
}

//
//  For abstracting over the dimension
//

abstract class NCell[A] {
  type Dim <: Nat

  val dim : Dim
  val cell : Cell[dim.Self, A]

  // def ev : Either[IsZero[dim.Self], HasPred[dim.Self]] =
  //   dim match {
  //     case Z => Left(new IsZero[dim.Self] { })
  //     case S(p) => Right(new HasPred[dim.Self] { type Pred = p.Self })
  //   }

  override def toString = cell.toString

  def zip[B](other : NCell[B]) : Option[NCell[(A, B)]] = {
    try {
      val o = other.cell.asInstanceOf[Cell[dim.Self, B]]
      for { res <- cell.zip(o) } yield res
    } catch {
      case e : Throwable => {
        println("Zip failed.")
        None
      }
    }
  }

  override def equals(that : Any) = {
    if (that.isInstanceOf[NCell[A]]) {
      cell == that.asInstanceOf[NCell[A]].cell
    } else {
      false
    }
  }
}

object NCell {

  implicit def cellIsNCell[D <: Nat, A](c : Cell[D, A]) : NCell[A] = 
      new NCell[A] {
        type Dim = D

        val dim = c.dimension
        val cell = c.asInstanceOf[Cell[dim.Self, A]]
      }

  implicit def ncellIsCell[A](ncell : NCell[A]) : Cell[ncell.dim.Self, A] =
      ncell.cell

  implicit def ncellHasOps[A](ncell : NCell[A]) : Cell.CellOps[ncell.dim.Self, A] = 
      ncell.cell

  def cellValueUpdate[D <: Nat](cell : Cell[D, String], newVal : String) : Cell[D, String] = 
    cell match {
      case Object(_, ev) => {
        implicit val isZero = ev
        Object(newVal).asInstanceOf[Cell[D, String]]
      }
      case Composite(_, srcTree, tgtVal, ev) => {
        implicit val hasPred = ev
        Composite(newVal, srcTree, tgtVal)
      }
    }

  def cellAgdaSyntax[D <: Nat](cell : Cell[D, String]) : String = 
    cell match {
      case Object(value, ev) => "id (" ++ value.toString ++ ")"
      case Composite(value, srcTree, tgtValue, ev) => cellTreeAgdaSyntax(srcTree)
    }

  def cellTreeAgdaSyntax[D <: Nat](tree : CellTree[D, String]) : String = {

    val perm : Vector[Int] = tree.inversePerm
    var index : Int = -1

    val addresses : Vector[HDN[D#Pred]] = 
      tree.dimension match {
        case Zero(ev) => Vector()
        case Succ(p, ev) => {
          implicit val hasPred = ev
          tree.flatten.hdn.nodeVector
        }
      }

    def addParens(str : String) : String = 
      if (str.contains(' ')) {
        "(" ++ str ++ ")"
      } else {
        str
      }

    def verticalTraverse(t : CellTree[D, String]) : String =
      t match {
        case Seed(obj, _) => "pt " ++ addParens(obj.toString)
        case Leaf(shape, _) => { index += 1 ; "leaf" } // ++ addresses(perm(index)).toAgda }
        case Graft(cell, branches, _) => {
          // Recurse on the branches, getting their string. then "plug these into" the result
          // by traversing the cells source tree.

          val verticalExprs = new Stack ++ (branches map (verticalTraverse(_)))

          cell match {
            case Object(value, _) => "node " ++ addParens(value.toString) ++ " " ++ addParens(verticalExprs.pop)
            case Composite(value, srcTree, tgtValue, _) => {

              def horizontalTraverse(tr : CellTree[D#Pred, String]) : CellTree[D#Pred, String] =
                tr match {
                  case Seed(o, ev) => {
                    implicit val isZero : IsZero[D#Pred] = ev
                    Seed(o map (_ => verticalExprs.pop)).asInstanceOf[CellTree[D#Pred, String]]
                  }
                  case Leaf(s, ev) => {
                    implicit val hasPred = ev
                    Leaf(s)
                  }
                  case Graft(c, brs, ev) => {
                    implicit val hasPred = ev
                    val newBranches = brs map (horizontalTraverse(_))
                    val localString = verticalExprs.pop
                    Graft(cellValueUpdate[D#Pred](c, localString), newBranches)
                  }
                }

              "node " ++ addParens(value.toString) ++ " " ++ addParens(cellTreeAgdaSyntax(horizontalTraverse(srcTree)))
            }
          }
        }
      }

    verticalTraverse(tree)

  }

  def cellScalaSyntax[D <: Nat](cell : Cell[D, String]) : String = 
    cell match {
      case Object(value, ev) => "Point(" ++ value.toString ++ ")"
      case Composite(value, srcTree, tgtValue, ev) => cellTreeScalaSyntax(srcTree)
    }

  def cellTreeScalaSyntax[D <: Nat](tree : CellTree[D, String]) : String = {

    val perm : Vector[Int] = tree.inversePerm
    var index : Int = -1

    val addresses : Vector[HDN[D#Pred]] = 
      tree.dimension match {
        case Zero(ev) => Vector()
        case Succ(p, ev) => {
          implicit val hasPred = ev
          tree.flatten.hdn.nodeVector
        }
      }

    def verticalTraverse(t : CellTree[D, String]) : String =
      t match {
        case Seed(obj, _) => "Pt(" ++ obj.toString ++ ")"
        case Leaf(shape, _) => { index += 1 ; "Leaf(__" ++ S(shape.dim).toInt.toString ++ ")" }
        case Graft(cell, branches, _) => {
          // Recurse on the branches, getting their string. then "plug these into" the result
          // by traversing the cells source tree.

          val verticalExprs = new Stack ++ (branches map (verticalTraverse(_)))

          cell match {
            case Object(value, _) => "Node(" ++ value.toString ++ ", " ++ verticalExprs.pop ++ ")"
            case Composite(value, srcTree, tgtValue, _) => {

              def horizontalTraverse(tr : CellTree[D#Pred, String]) : CellTree[D#Pred, String] =
                tr match {
                  case Seed(o, ev) => {
                    implicit val isZero : IsZero[D#Pred] = ev
                    Seed(o map (_ => verticalExprs.pop)).asInstanceOf[CellTree[D#Pred, String]]
                  }
                  case Leaf(s, ev) => {
                    implicit val hasPred = ev
                    Leaf(s)
                  }
                  case Graft(c, brs, ev) => {
                    implicit val hasPred = ev
                    val newBranches = brs map (horizontalTraverse(_))
                    val localString = verticalExprs.pop
                    Graft(cellValueUpdate[D#Pred](c, localString), newBranches)
                  }
                }

              "Node(" ++ value.toString ++ "," ++ cellTreeScalaSyntax(horizontalTraverse(srcTree)) ++ ")"
            }
          }
        }
      }

    verticalTraverse(tree)

  }

  def cellTreeXmlSyntax[D <: Nat](tree : CellTree[D, String]) : String = {

    val perm : Vector[Int] = tree.inversePerm
    var index : Int = -1

    val addresses : Vector[HDN[D#Pred]] = 
      tree.dimension match {
        case Zero(ev) => Vector()
        case Succ(p, ev) => {
          implicit val hasPred = ev
          tree.flatten.hdn.nodeVector
        }
      }

    def verticalTraverse(t : CellTree[D, String]) : String =
      t match {
        case Seed(obj, _) => "<point>" ++ obj.toString ++ "</point>"
        case Leaf(shape, _) => { index += 1 ; "<leaf>" ++ addresses(perm(index)).toXml ++ "</leaf>" }
        case Graft(cell, branches, _) => {
          // Recurse on the branches, getting their string. then "plug these into" the result
          // by traversing the cells source tree.

          val verticalExprs = new Stack ++ (branches map (verticalTraverse(_)))

          cell match {
            case Object(value, _) => "<node><value>" ++ value.toString ++ "</value><shell>" ++ verticalExprs.pop ++ "</shell></node>"
            case Composite(value, srcTree, tgtValue, _) => {

              def horizontalTraverse(tr : CellTree[D#Pred, String]) : CellTree[D#Pred, String] =
                tr match {
                  case Seed(o, ev) => {
                    implicit val isZero : IsZero[D#Pred] = ev
                    Seed(o map (_ => verticalExprs.pop)).asInstanceOf[CellTree[D#Pred, String]]
                  }
                  case Leaf(s, ev) => {
                    implicit val hasPred = ev
                    Leaf(s)
                  }
                  case Graft(c, brs, ev) => {
                    implicit val hasPred = ev
                    val newBranches = brs map (horizontalTraverse(_))
                    val localString = verticalExprs.pop
                    Graft(cellValueUpdate[D#Pred](c, localString), newBranches)
                  }
                }

              "<node><value>" ++ value.toString ++ "</value><shell>" ++ cellTreeXmlSyntax(horizontalTraverse(srcTree)) ++ "</shell></node>"
            }
          }
        }
      }

    verticalTraverse(tree)

  }

  def agdaSyntax[A](ncell : NCell[A]) : String = 
    ncell.cell.regenerateFrom(new PdGenerator[A]).targetValues.mkString("\n", "\n\n", "\n")

  def scalaSyntax[A](ncell : NCell[A]) : String = 
    ncell.cell.regenerateFrom(new PdScalaGenerator[A]).targetValues.mkString("\n", "\n\n", "\n")

  def xmlSyntax[A](ncell : NCell[A]) : String = {
    var curDim : Int = -1

    val tgts = ncell.cell.regenerateFrom(new PdXmlGenerator[A]).targetValues

    val results = tgts.reverse map (s => {
      curDim += 1
      "<pd dimension=\"" ++ curDim.toString ++ "\">" ++ s ++ "</pd>"
    })

    results.mkString("<complex>", "", "</complex>")

  }

  object TargetInserter extends CellRegenerator[String, String] {
    def generateObject[D <: Nat : IsZero](lbl : String) : Cell[D, String] = {
      Object(lbl).asInstanceOf[Cell[D, String]]
    }

    def generateCell[D <: Nat : HasPred](cellLbl : String, srcs : CellTree[D#Pred, String], tgtLbl : String) = {
      Composite("(" ++ cellLbl ++ " , " ++ tgtLbl ++ ")", srcs, tgtLbl)
    }
  }

  class PdGenerator[-A] extends CellRegenerator[A, String] {

    def generateObject[D <: Nat : IsZero](lbl : A) : Cell[D, String] = {
      ObjectCell("obj " ++ lbl.toString)
    }

    def generateCell[D <: Nat : HasPred](cellLbl : A, srcs : CellTree[D#Pred, String], tgtLbl : A) = {

      val lblString = "ext " ++ cellLbl.toString // ++ " (" ++ cellTreeAgdaSyntax(srcs.addrTree map (_.toAgda)) ++ ")"
      val tgtString = "int " ++ tgtLbl.toString ++ " (" ++ cellTreeAgdaSyntax(srcs) ++ ")"

      CompositeCell(lblString, srcs, tgtString)
    }

  }

  class PdScalaGenerator[-A] extends CellRegenerator[A, String] {

    def generateObject[D <: Nat : IsZero](lbl : A) : Cell[D, String] = {
      ObjectCell("Obj(" ++ lbl.toString ++")")
    }

    def generateCell[D <: Nat : HasPred](cellLbl : A, srcs : CellTree[D#Pred, String], tgtLbl : A) = {

      val lblString = "Dot(" ++ cellLbl.toString ++ ", __" ++ S(srcs.dimension).toInt.toString ++ ")" // ", " ++ cellTreeScalaSyntax(srcs.addrTree map (_.toScala)) ++ ")"
      val tgtString = "Box(" ++ tgtLbl.toString ++ ", " ++ cellTreeScalaSyntax(srcs) ++ ")"

      CompositeCell(lblString, srcs, tgtString)
    }

  }

  class PdXmlGenerator[-A] extends CellRegenerator[A, String] {

    def generateObject[D <: Nat : IsZero](lbl : A) : Cell[D, String] = {
      ObjectCell("<obj>" ++ lbl.toString ++ "</obj>")
    }

    def generateCell[D <: Nat : HasPred](cellLbl : A, srcs : CellTree[D#Pred, String], tgtLbl : A) = {

      val lblString = "<dot><label>" ++ cellLbl.toString ++ "</label><corolla>" ++ cellTreeXmlSyntax(srcs.addrTree map (_.toXml)) ++ "</corolla></dot>"
      val tgtString = "<box><label>" ++ tgtLbl.toString ++ "</label><shell>" ++ cellTreeXmlSyntax(srcs) ++ "</shell></box>"

      CompositeCell(lblString, srcs, tgtString)
    }

  }

}

//
//  For doing source-tree dependent mappings ...
//

trait CellRegenerator[-A, B] {
  def generateObject[D <: Nat : IsZero](lbl : A) : Cell[D, B]
  def generateCell[D <: Nat : HasPred](cellLbl : A, srcs : CellTree[D#Pred, B], tgtLbl : A) : Cell[D, B]
}

object CellRegenerator {
  def mapRegenerator[A, B](f : A => B) = 
    new CellRegenerator[A, B] {
      def generateObject[D <: Nat : IsZero](lbl : A) = ObjectCell(f(lbl))
      def generateCell[D <: Nat : HasPred](cellLbl : A, srcs : CellTree[D#Pred, B], tgtLbl : A) =
          CompositeCell(f(cellLbl), srcs, f(tgtLbl))
    }
}
