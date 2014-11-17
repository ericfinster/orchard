/**
  * IndexedComplex.scala - A Complex with a parameterized label type
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds

import scalaz.{Tree => _, Zipper => _, _}
import scalaz.std.option._

import Nats._
import Tree._

object IndexedComplex {

  import IndexedZipper._

  type IndexedComplex[N <: Nat, A[_ <: Nat]] = Suite[Nesting, N, A[N]]

}

object IndexedZipper {

  import Suite._
  import IndexedComplex._

  type IndexedZipper[N <: Nat, A[_ <: Nat]] = Suite[NestingZipper, N, A[N]]

  //============================================================================================
  // SEAL
  //

  // def seal[N <: Nat, A[_ <: Nat]](cz : IndexedZipper[N, A]) : IndexedComplex[N, A] = ???
  // //   SealRecursor.execute(cz.dim)(cz)

  // type SealIn[N <: Nat, A[_ <: Nat]] = IndexedZipper[N, A]
  // type SealOut[N <: Nat, A[_ <: Nat]] = IndexedComplex[N, A]

  // object SealRecursor extends NatRecursorT1P1[SealIn, SealOut] {

  //   def caseZero[A[_ <: Nat]](cz : IndexedZipper[_0, A]) : IndexedComplex[_0, A] = 
  //     cz.head.close

  //   def caseSucc[P <: Nat, A[_ <: Nat]](cz : IndexedZipper[S[P], A]) : IndexedComplex[S[P], A] = 
  //     seal(tailOf(cz)) >> cz.head.close

  // }

}
