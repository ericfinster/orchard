/**
  * Bindings.scala - Binding monad
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz._
import StateT._
import Kleisli._

import orchard.core.util._
import orchard.core.cell._
import ErrorM._
import MonadUtils._

trait Bindings { thisChecker : TypeChecker =>

  type BinderState = Map[String, String]

  type Binder[+A] = StateT[Checker, BinderState, A]
  type BinderT[M[+_], A] = StateT[M, BinderState, A]
  type BinderS[S, A] = StateT[Checker, S, A]

  val BinderMS = MonadState[BinderS, BinderState]
  val BinderMT = MonadTrans[BinderT]

  object BindingConversions {
    implicit def fromChecker[A](chckr : Checker[A]) : Binder[A] = BinderMT.liftM(chckr)
  }

  import BinderMS._
  import BindingConversions._
  import CheckerMR._
  import CheckerErrorSyntax._

  // Now here we want to start implementing the binding algorithm ....

  def bind(site : CellExpression,  overlay : CellExpression) : Binder[Boolean] = 
    for {
      env <- ask

      concreteSize <- resolve(site)
      concreteOverlay <- resolve(overlay)

      // zippedExpr <- attempt(
      //   fromOption(
      //     src.ncell.zip(tgt.ncell),
      //     "Binding error: cells have incompatible shape"
      //   )
      // )

      // // Now, we can map over the given cells and generate a set of requirements.  This
      // // will include the subbindings which must be satisfied as well 
      // _ = zippedExpr map {
      //   case (s, t) => ???
      // }

    } yield true

}
