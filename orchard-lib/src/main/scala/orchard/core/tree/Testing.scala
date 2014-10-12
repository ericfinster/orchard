/**
  * Testing.scala - Some testing
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.{Tree => _, Zipper => _, _}

import Trees._
import Nats._

object Testing {

  // def implicitTest[N <: Nat, A](tr : Tree[N, A])(implicit n : N) : Unit = {

  //   val tfns : TreeFunctions[N] = implicitly[TreeFunctions[N]]
  //   val trvTest : Traverse[N#Tree] = implicitly[Traverse[N#Tree]]

  //   val test = implicitly[TreeType[Tree[N, A]]]
  //   val test2 : Unit = tr.opsTest

  //   ()
  // }

}
