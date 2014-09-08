/**
  * Experimental.scala - Experiments with Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions

import Nats._

object Experimental {

  trait TreeSpecs[N <: Nat] {

    type Tree[+_]
    type Cardinal[+_]

    type Context[+_]
    type Derivative[+_]

  }

  object TreeZeroSpecs extends TreeSpecs[_0] {

    type Tree[+A] = A
    type Cardinal[+A] = A

    type Context[+A] = Unit
    type Derivative[+A] = Unit

  }

  trait TreeSuccSpecs[N <: Nat] extends TreeSpecs[S[N]] {

    type Tree[+A] = Slice[TreeSpecs[N]#Tree, A]
    type Cardinal[+A] = TreeSpecs[N]#Cardinal[Tree[A]]

  }

  type Tree0[+A] = A
  type Tree1[+A] = Slice[Tree0, A]
  type Tree2[+A] = Slice[Tree1, A]
  type Tree3[+A] = Slice[Tree2, A]
  type Tree4[+A] = Slice[Tree3, A]

  type Tree[N <: Nat, +A] = TreeSpecs[N]#Tree[A]

}
