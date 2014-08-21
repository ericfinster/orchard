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

import scalaz.syntax.traverse._

import orchard.core.util._
import orchard.core.cell._
import ErrorM._
import MonadUtils._

trait Bindings { thisChecker : TypeChecker =>

  type BinderState = Map[Variable, CellExpression]

  type Binder[+A] = StateT[Checker, BinderState, A]
  type BinderT[M[+_], A] = StateT[M, BinderState, A]
  type BinderS[S, A] = StateT[Checker, S, A]

  val BinderMS = MonadState[BinderS, BinderState]
  val BinderMT = MonadTrans[BinderT]

  object BindingConversions {
    def fromChecker[A](chckr : Checker[A]) : Binder[A] = BinderMT.liftM(chckr)
  }

  import BinderMS._
  import BindingConversions._
  import CheckerErrorSyntax._

  // Now here we want to start implementing the binding algorithm ....

  def bind(variable : Variable, expr : CellExpression) : Binder[Unit] = 
    for {

      currentBindings <- get

      _ <- (

        if (currentBindings.isDefinedAt(variable)) {
          fromChecker(ensureConvertible(currentBindings(variable), expr))
        } else {
          for {

            // Make sure the universality condition is ok
            _ <- fromChecker(
              attempt(
                ensure(
                  if (variable.isThin) expr.isThin else true,
                  "Attempt to bind a non-thin expression to a thin variable"
                )
              )
            )

            variableShell = variable.shell.ncell
            exprNCell = expr.ncell

            matchedShape <- fromChecker(
              attempt(
                fromOption(
                  variableShell.zip(exprNCell),
                  "Binding error: cells have incompatible shape"
                )
              )
            )

            _ <- {

              // Now, we map over the matchedShape and check if the pairs are
              // convertible, or if we meet a variable on the shell, we try to
              // bind it as well.

              val shellBinding : NCell[Binder[Unit]] = 
                matchedShape map {

                  // case (Empty, _) => point(())
                  // case (Full(varSubExpr), subExpr) =>
                  //   for {
                  //     shellExpr <- fromChecker(resolve(varSubExpr))

                  //     _ <- shellExpr match {
                  //       case v : Variable => bind(v, subExpr)
                  //       case _ => fromChecker(
                  //         ensureConvertible(shellExpr, subExpr)
                  //       )
                  //     }
                  //   } yield ()

                  ???

                }

              // Run all the shell bindings ...
              shellBinding.sequence[Binder, Unit]
            }

            // Everything was okay, go ahead and bind ...
            _ <- put(currentBindings + (variable -> expr))

          } yield ()
        }

      )

    } yield ()


}
