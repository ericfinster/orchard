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
import Suite._
import Nesting._
import Complex._

object ComplexZipper {

  type ComplexZipper[N <: Nat, A] = Suite[NestingZipper, N, A]

  //============================================================================================
  // SEAL
  //

  def seal[N <: Nat, A](cz : ComplexZipper[N, A]) : Complex[N, A] = 
    SealRecursor.execute(cz.dim)(cz)

  type SealIn[N <: Nat, A] = ComplexZipper[N, A]
  type SealOut[N <: Nat, A] = Complex[N, A]

  object SealRecursor extends NatRecursorT1P1[SealIn, SealOut] {

    def caseZero[A](cz : ComplexZipper[_0, A]) : Complex[_0, A] = 
      cz.head.close

    def caseSucc[P <: Nat, A](cz : ComplexZipper[S[P], A]) : Complex[S[P], A] = 
      seal(tailOf(cz)) >> cz.head.close

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
      for {
        zp <- NestingZipper.visit(dir, cz.head)
      } yield zp

    def caseSucc[P <: Nat, A](dir : Direction[S[S[P]]], cz : ComplexZipper[S[P], A]) : Option[ComplexZipper[S[P], A]] = 
      dir match {
        case Dir(Root()) => 
          for {
            zp <- NestingZipper.visit(dir, cz.head)
          } yield tailOf(cz) >> zp

        case Dir(Step(d, ds)) => 
          for {

            prefixZipper <- caseSucc(Dir(ds), cz)
            prefixNestingZipper = prefixZipper.head
            nestingSibling <- NestingZipper.sibling(d, prefixNestingZipper)
            prefixSpine <- Nesting.spine(prefixNestingZipper.focus)

            res <- (
              prefixSpine match {
                case Leaf(_) => Some(tailOf(prefixZipper) >> nestingSibling)
                case Node(_, shell) => 
                  for {
                    extents <- Tree.shellExtents(shell)
                    recAddr <- Tree.valueAt(extents, d)
                    fixupLower <- seek(recAddr, tailOf(prefixZipper))
                  } yield (fixupLower >> nestingSibling)
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
      NestingZipper(cmplx.head, Bottom())

    def caseSucc[P <: Nat, A](cmplx : Complex[S[P], A]) : ComplexZipper[S[P], A] = 
      fromComplex(tailOf(cmplx)) >> NestingZipper(cmplx.head, Bottom())

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
      cz.head.withFocus(nst)

    def caseSucc[P <: Nat, A](cz : ComplexZipper[S[P], A], nst : Nesting[S[P], A]) : ComplexZipper[S[P], A] = 
      tailOf(cz) >> cz.head.withFocus(nst)

  }

  def focusValue[N <: Nat, A](cz : ComplexZipper[N, A]) : A = 
    labelOf(cz.head.focus)

  def focusCorolla[N <: Nat, A](cz : ComplexZipper[S[N], A]) : Option[Tree[N, Address[N]]] = 
    FocusCorollaRecursor.execute(tailOf(cz).dim)(cz)

  type FocusCorollaIn[N <: Nat, A] = ComplexZipper[S[N], A]
  type FocusCorollaOut[N <: Nat, A] = Option[Tree[N, Address[N]]]

  object FocusCorollaRecursor extends NatRecursorT1P1[FocusCorollaIn, FocusCorollaOut] {

    def caseZero[A](cz : ComplexZipper[_1, A]) : Option[Tree[_0, Address[_0]]] = 
      Some(Pt(Root()))

    def caseSucc[P <: Nat, A](cz : ComplexZipper[S[S[P]], A]) : Option[Tree[S[P], Address[S[P]]]] = 
      cz match {
        case (tl >> hd) =>
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
      Some(Pt(cz.head.focus))

    def caseSucc[P <: Nat, A](cz : ComplexZipper[S[P], A]) : Option[Tree[S[P], Nesting[S[P], A]]] = 
      for {
        cr <- focusCorolla(cz)
      } yield Node(cz.head.focus, map(cr)(Leaf(_)))

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
        case (_ >> NestingZipper(Dot(_, _), _)) => None
        case (_ >> NestingZipper(Box(_, int), _)) => Some(addressTree(int))
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
      Some(cz.head.withContext(Bottom()))

    def caseSucc[P <: Nat, A](cz : ComplexZipper[S[P], A]) : Option[ComplexZipper[S[P], A]] = {

      type SrcM[R] = SourceM[P, A, R]
      type SrcS[S, R] = StateT[Option, S, R]

      val MS = MonadState[SrcS, Complex[P, A]]
      import MS._

      val nst = cz.head.focus

      for {
        fnst <- spine(nst)
        newTail <- restrictFocus(tailOf(cz))
        newComplex <- exciseLocal(Root()(fnst.dim), fnst).exec(seal(newTail))
      } yield (fromComplex(newComplex) >> NestingZipper(nst, Bottom()))

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
        case Base(NestingZipper(fcs, cn)) =>
          Some(NestingZipper(Obj(labelOf(fcs)), cn))
      }

    def caseSucc[P <: Nat, A](cz : ComplexZipper[S[P], A]) : Option[ComplexZipper[S[P], A]] =
      cz match {
        case (tl >> NestingZipper(nst, cn)) =>
          for {
            fnst <- spine(nst)
            newTl <- compressFocus(tl, fnst)
            cr <- focusCorolla(cz)
          } yield (newTl >> NestingZipper(Dot(labelOf(nst), cr), cn))
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
