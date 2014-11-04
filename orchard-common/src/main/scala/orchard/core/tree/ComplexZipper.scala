/**
  * ComplexZipper.scala - Zippers for Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scalaz.{Tree => _, Zipper => _, _}
import scalaz.std.option._

import Nats._
import Tree._
import Nesting._
import Complex._

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
  // SEAL
  //

  def seal[N <: Nat, A](cz : ComplexZipper[N, A]) : Complex[N, A] = 
    SealRecursor.execute(cz.dim)(cz)

  type SealIn[N <: Nat, A] = ComplexZipper[N, A]
  type SealOut[N <: Nat, A] = Complex[N, A]

  object SealRecursor extends NatRecursorT1P1[SealIn, SealOut] {

    def caseZero[A](cz : ComplexZipper[_0, A]) : Complex[_0, A] = 
      Base(headOf(cz).close)

    def caseSucc[P <: Nat, A](cz : ComplexZipper[S[P], A]) : Complex[S[P], A] = 
      Append(seal(tailOf(cz)) , headOf(cz).close)

  }

  //============================================================================================
  // VISIT
  //

  def visit[N <: Nat, A](addr : Direction[S[N]], cz : ComplexZipper[N, A]) : Option[ComplexZipper[N, A]] = 
    VisitRecursor.execute(cz.dim)(addr, cz)

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

  //============================================================================================
  // FROM COMPLEX
  //

  def fromComplex[N <: Nat, A](cmplx : Complex[N, A]) : ComplexZipper[N, A] =
    FromComplexRecursor.execute(cmplx.dim)(cmplx)

  type FromComplexIn[N <: Nat, A] = Complex[N, A]
  type FromComplexOut[N <: Nat, A] = ComplexZipper[N, A]

  object FromComplexRecursor extends NatRecursorT1P1[FromComplexIn, FromComplexOut] {

    def caseZero[A](cmplx : Complex[_0, A]) : ComplexZipper[_0, A] = 
      cmplx match {
        case Base(objNst) => ObjZipper(NestingZipper(objNst, Bottom()))
      }

    def caseSucc[P <: Nat, A](cmplx : Complex[S[P], A]) : ComplexZipper[S[P], A] = 
      cmplx match {
        case Append(tl, hd) => DiagramZipper(fromComplex(tl), NestingZipper(hd, Bottom()))
      }

  }

  //============================================================================================
  // FOCUS ROUTINES
  //

  def updateFocus[N <: Nat, A](cz : ComplexZipper[N, A], nst : Nesting[N, A]) : ComplexZipper[N, A] = 
    UpdateFocusRecursor.execute(cz.dim)(cz, nst)

  type UpdateFocusIn0[N <: Nat, A] = ComplexZipper[N, A]
  type UpdateFocusIn1[N <: Nat, A] = Nesting[N, A]
  type UpdateFocusOut[N <: Nat, A] = ComplexZipper[N, A]

  object UpdateFocusRecursor extends NatRecursorT1P2[UpdateFocusIn0, UpdateFocusIn1, UpdateFocusOut] {

    def caseZero[A](cz : ComplexZipper[_0, A], nst : Nesting[_0, A]) : ComplexZipper[_0, A] = 
      cz match {
        case ObjZipper(oz) => ObjZipper(oz.withFocus(nst))
      }

    def caseSucc[P <: Nat, A](cz : ComplexZipper[S[P], A], nst : Nesting[S[P], A]) : ComplexZipper[S[P], A] = 
      cz match {
        case DiagramZipper(tl, hd) => DiagramZipper(tl, hd.withFocus(nst))
      }

  }

  def focusValue[N <: Nat, A](cz : ComplexZipper[N, A]) : A = 
    labelOf(headOf(cz).focus)

  def focusCorolla[N <: Nat, A](cz : ComplexZipper[S[N], A]) : Option[Tree[N, Address[N]]] = 
    FocusCorollaRecursor.execute(tailOf(cz).dim)(cz)

  type FocusCorollaIn[N <: Nat, A] = ComplexZipper[S[N], A]
  type FocusCorollaOut[N <: Nat, A] = Option[Tree[N, Address[N]]]

  object FocusCorollaRecursor extends NatRecursorT1P1[FocusCorollaIn, FocusCorollaOut] {

    def caseZero[A](cz : ComplexZipper[_1, A]) : Option[Tree[_0, Address[_0]]] = 
      Some(Pt(Root()))

    def caseSucc[P <: Nat, A](cz : ComplexZipper[S[S[P]], A]) : Option[Tree[S[P], Address[S[P]]]] = 
      cz match {
        case DiagramZipper(tl, hd) =>
          for {
            sp <- Nesting.spine(hd.focus)
            result <- (
              sp match {
                case Leaf(_) =>
                  for {
                    cr <- focusCorolla(tl)
                  } yield Node(Root()(tl.dim), map(cr)(Leaf(_)))
                case Node(_, sh) => Tree.shellCorolla(sh)
              }
            )
          } yield result
      }

  }

  def focusUnit[N <: Nat, A](cz : ComplexZipper[N, A]) : Option[Tree[N, Nesting[N, A]]] = 
    FocusUnitRecursor.execute(cz.dim)(cz)

  type FocusUnitIn[N <: Nat, A] = ComplexZipper[N, A]
  type FocusUnitOut[N <: Nat, A] = Option[Tree[N, Nesting[N, A]]]

  object FocusUnitRecursor extends NatRecursorT1P1[FocusUnitIn, FocusUnitOut] {

    def caseZero[A](cz : ComplexZipper[_0, A]) : Option[Tree[_0, Nesting[_0, A]]] = 
      Some(Pt(headOf(cz).focus))

    def caseSucc[P <: Nat, A](cz : ComplexZipper[S[P], A]) : Option[Tree[S[P], Nesting[S[P], A]]] = 
      for {
        cr <- focusCorolla(cz)
      } yield Node(headOf(cz).focus, map(cr)(Leaf(_)))

  }

  def focusCanopy[N <: Nat, A](cz : ComplexZipper[N, A]) : Option[Tree[N, Address[N]]] = 
    FocusCanopyRecursor.execute(cz.dim)(cz)

  type FocusCanopyIn[N <: Nat, A] = ComplexZipper[N, A]
  type FocusCanopyOut[N <: Nat, A] = Option[Tree[N, Address[N]]]

  object FocusCanopyRecursor extends NatRecursorT1P1[FocusCanopyIn, FocusCanopyOut] {

    def caseZero[A](cz : ComplexZipper[_0, A]) : Option[Tree[_0, Address[_0]]] =
      Some(Pt(Root()))

    def caseSucc[P <: Nat, A](cz : ComplexZipper[S[P], A]) : Option[Tree[S[P], Address[S[P]]]] = 
      cz match {
        case DiagramZipper(_, NestingZipper(Dot(a, cr), cn)) => None
        case DiagramZipper(_, NestingZipper(Box(a, int), cn)) => Some(addressTree(int))
      }

  }

  //============================================================================================
  // RESTRICT FOCUS
  //

  def restrictFocus[N <: Nat, A](cz : ComplexZipper[N, A]) : Option[ComplexZipper[N, A]] = 
    RestrictFocusRecursor.execute(cz.dim)(cz)

  type RestrictFocusIn[N <: Nat, A] = ComplexZipper[N, A]
  type RestrictFocusOut[N <: Nat, A] = Option[ComplexZipper[N, A]]

  object RestrictFocusRecursor extends NatRecursorT1P1[RestrictFocusIn, RestrictFocusOut] {

    def caseZero[A](cz : ComplexZipper[_0, A]) : Option[ComplexZipper[_0, A]] = 
      Some(ObjZipper(headOf(cz).withContext(Bottom())))

    def caseSucc[P <: Nat, A](cz : ComplexZipper[S[P], A]) : Option[ComplexZipper[S[P], A]] = {

      type SrcM[R] = SourceM[P, A, R]
      type SrcS[S, R] = StateT[Option, S, R]

      val MS = MonadState[SrcS, Complex[P, A]]
      import MS._

      val tl = tailOf(cz)
      val nst = headOf(cz).focus
      val cn = headOf(cz).context

      for {
        fnst <- spine(nst)
        newTail <- restrictFocus(tl)
        newComplex <- exciseLocal(Root()(fnst.dim), fnst).exec(seal(newTail))
      } yield DiagramZipper(fromComplex(newComplex), NestingZipper(nst, Bottom()))
    }

  }

  //============================================================================================
  // CONTRACT FOCUS
  //

  def contractFocus[N <: Nat, A](cz : ComplexZipper[N, A]) : Option[ComplexZipper[N, A]] = 
    ContractFocusRecursor.execute(cz.dim)(cz)

  type ContractFocusIn[N <: Nat, A] = ComplexZipper[N, A]
  type ContractFocusOut[N <: Nat, A] = Option[ComplexZipper[N, A]]

  object ContractFocusRecursor extends NatRecursorT1P1[ContractFocusIn, ContractFocusOut] {

    def caseZero[A](cz : ComplexZipper[_0, A]) : Option[ComplexZipper[_0, A]] = 
      cz match {
        case ObjZipper(NestingZipper(fcs, cn)) =>
          Some(ObjZipper(NestingZipper(Obj(labelOf(fcs)), cn)))
      }

    def caseSucc[P <: Nat, A](cz : ComplexZipper[S[P], A]) : Option[ComplexZipper[S[P], A]] =
      cz match {
        case DiagramZipper(tl, NestingZipper(nst, cn)) =>
          for {
            fnst <- spine(nst)
            newTl <- compressFocus(tl, fnst)
            cr <- focusCorolla(cz)
          } yield DiagramZipper(newTl, NestingZipper(Dot(labelOf(nst), cr), cn))
      }

  }

  //============================================================================================
  // COMPRESS FOCUS
  //

  def compressFocus[N <: Nat, A](cz : ComplexZipper[N, A], tr : Tree[S[N], A]) : Option[ComplexZipper[N, A]] =
    for {
      nst <- compressLocal(cz, tr)
    } yield updateFocus(cz, Box(focusValue(cz), nst))

  def compressLocal[N <: Nat, A](cz : ComplexZipper[N, A], tr : Tree[S[N], A]) : Option[Tree[N, Nesting[N, A]]] = 
    tr match {
      case Leaf(_) => focusUnit(cz)
      case Node(_, sh) => {

        import scalaz.std.option._

        for {
          cp <- focusCanopy(cz)
          zsh <- Tree.zipComplete(cp, sh)
          toJoin <- traverse(zsh)({ case (d, t) => 
            for {
              cz0 <- visit(d, cz)
              lr <- compressLocal(cz0, t)
            } yield lr
          })
          result <- Tree.join(toJoin)
        } yield result
      }

    }

}
