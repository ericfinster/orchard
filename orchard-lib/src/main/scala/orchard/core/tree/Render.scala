/**
  * Render.scala - Rendering Routines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scalaz.{Tree => _, Zipper => _, _}

import Nats._
import Trees._
import PastingDiagrams._

trait Sized {

  def width : Double
  def height : Double

}

trait Sizeable[A] {

  def widthOf(a : A) : Double
  def heightOf(a : A) : Double

}

abstract class LayoutMarker {

  var rootX : Double
  var rootY : Double

  var hdeps : List[LayoutMarker]
  var vdeps : List[LayoutMarker]

}

abstract class RenderData[A : Sizeable] {

  def rootX : Double
  def rootY : Double

}

abstract class Renderer {

  def edgeWidth : Double
  def internalPadding : Double
  def externalPadding : Double

  def render[A : Sizeable](op : Opetope[A]) : Opetope[RenderData[A]] 

  def renderObjectStack[A : Sizeable](pd : Pd[_0, A]) : Pd[_0, RenderData[A]] = 
    pd match {
      case Obj(a) => ???
      case Box(a, canopy) => ???
    }

  def renderNesting[N <: Nat, A : Sizeable](pd : Pd[S[N], A], lvs : Tree[N, RenderData[A]])(implicit n : N) : Pd[S[N], RenderData[A]] = {

    pd match {
      case Dot(a, corolla) => {
        ???
      }
      case Box(a, Leaf(addr)) => {
        // This is a drop
        ???
      }
      case Box(a, Node(pd, verts)) => {
        // We should recurse
        ???
      }
    }

  }

  def renderVertical[N <: Nat, A : Sizeable](tr : Tree[S[N], Pd[S[N], A]], lvs : Tree[N, RenderData[A]])(implicit n : N) : Pd[S[N], RenderData[A]] =
    tr match {
      case Leaf(addr) => ???
      case Node(pd, verts) => {

        //implicit def getTraverse : Traverse[N#Tree] = treeIsTraverse[N]

        // val T : Traverse[N#Tree] = implicitly[Traverse[N#Tree]] // treeIsTraverse[N]

        // implicitly[Traverse[N#Tree]]

        // val test = verts map (_ => 3)

        ???
      }
    }

}
