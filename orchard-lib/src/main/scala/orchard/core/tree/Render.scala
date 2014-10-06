/**
  * Render.scala - Rendering Routines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scalaz.{Tree => _, Zipper => _, _}

import Nats._
import Slice._
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

abstract class LayoutMarker

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

  def renderNesting[N <: Nat, A : Sizeable](pd : Pd[S[N], A], lvs : Tree[N, LayoutMarker])(implicit n : N) : Pd[S[N], RenderData[A]] = {

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

}
