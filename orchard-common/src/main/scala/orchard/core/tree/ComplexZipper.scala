/**
  * ComplexZipper.scala - Zippers for Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import Nats._
import Tree._
import Nesting._

sealed abstract class ComplexZipper[N <: Nat, +A] { def dim : N }
case class ObjZipper[+A](objZipper : NestingZipper[_0, A]) extends ComplexZipper[_0, A] { def dim = Z }
case class DiagramZipper[N <: Nat, +A](cmplxZipper : ComplexZipper[N, A], nstZipper : NestingZipper[S[N], A]) 
  extends ComplexZipper[S[N], A] { def dim = nstZipper.dim }

object ComplexZipper {

  //============================================================================================
  // HEAD OF
  //

  def headOf[N <: Nat, A](cz : ComplexZipper[N, A]) : NestingZipper[N, A] = 
    HeadRecursor.execute(cz.dim)(cz)

  type HeadIn[N <: Nat, A] = ComplexZipper[N, A]
  type HeadOut[N <: Nat, A] = NestingZipper[N, A]

  object HeadRecursor extends NatRecursorT1P1[HeadIn, HeadOut] {

    def caseZero[A](cz : ComplexZipper[_0, A]) : NestingZipper[_0, A] = 
      cz match {
        case ObjZipper(hd) => hd
      }

    def caseSucc[P <: Nat, A](cz : ComplexZipper[S[P], A]) : NestingZipper[S[P], A] =
      cz match {
        case DiagramZipper(_, hd) => hd
      }

  }

  //============================================================================================
  // TAIL OF
  //

  def tailOf[N <: Nat, A](cz : ComplexZipper[S[N], A]) : ComplexZipper[N, A] = 
    cz match {
      case DiagramZipper(tl, _) => tl
    }

  //============================================================================================
  // VISIT
  //

  def visit[N <: Nat, A](addr : Direction[S[N]], cz : ComplexZipper[N, A]) : Option[ComplexZipper[N, A]] = ???

  type VisitIn0[N <: Nat, A] = Direction[S[N]]
  type VisitIn1[N <: Nat, A] = ComplexZipper[N, A]
  type VisitOut[N <: Nat, A] = Option[ComplexZipper[N, A]]

  object VisitRecursor extends NatRecursorT1P2[VisitIn0, VisitIn1, VisitOut] {

    def caseZero[A](dir : Direction[_1], cz : ComplexZipper[_0, A]) =
      cz match {
        case ObjZipper(oz) => 
          for {
            zp <- NestingZipper.visit(dir, oz)
          } yield ObjZipper(zp)
      }

    def caseSucc[P <: Nat, A](dir : Direction[S[S[P]]], cz : ComplexZipper[S[P], A]) : Option[ComplexZipper[S[P], A]] = 
      dir match {
        case Wrap(Root()) => 
          for {
            zp <- NestingZipper.visit(dir, headOf(cz))
          } yield DiagramZipper(tailOf(cz), zp)
        case Wrap(Step(d, ds)) => 
          for {

            prefixZipper <- caseSucc(Wrap(ds), cz)
            prefixNestingZipper = headOf(prefixZipper)
            nestingSibling <- NestingZipper.sibling(d, prefixNestingZipper)
            prefixSpine <- Nesting.spine(prefixNestingZipper.focus)

            res <- (
              prefixSpine match {
                case Leaf(_) => Some(DiagramZipper(tailOf(prefixZipper), nestingSibling))
                case Node(_, shell) => 
                  for {
                    extents <- Tree.shellExtents(shell)
                    recAddr <- Tree.valueAt(extents, d)
                    fixupLower <- seek(recAddr, tailOf(prefixZipper))
                  } yield DiagramZipper(fixupLower, nestingSibling)
              }
            )

          } yield res
      }

  }

  //============================================================================================
  // SEEK
  //

  def seek[N <: Nat, A](addr : Address[S[N]], cz : ComplexZipper[N, A]) : Option[ComplexZipper[N, A]] =
    addr match {
      case Root() => Some(cz)
      case Step(d, ds) =>
        for {
          zp <- seek(ds, cz)
          zr <- visit(d, zp)
        } yield zr
    }


}
