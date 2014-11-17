/**
  * Address.scala - Tree Addresses
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.implicitConversions

import Nats._

sealed trait Direction[N <: Nat] { def dim : N }
case class Dir[N <: Nat](addr : Address[N]) extends Direction[S[N]] { def dim = S(addr.dim) }

object Direction {

  implicit def succDirIsAddress[N <: Nat](dir : Direction[S[N]]) : Address[N] = 
    dir match {
      case Dir(addr) => addr
    }

  implicit def addrIsSuccDirction[N <: Nat](addr : Address[N]) : Direction[S[N]] = 
    Dir(addr)

}

sealed trait Address[N <: Nat] { def dim : N }
case class Root[N <: Nat](implicit val n : N) extends Address[N] { def dim = n }
case class Step[N <: Nat](dir : Direction[N], addr : Address[N]) extends Address[N] { def dim = addr.dim }

// abstract class FaceAddress[N <: Nat] {

//   type Dim <: Nat

//   val address : Address[S[Dim]]
//   val gte : Gte[N, Dim]

// }

