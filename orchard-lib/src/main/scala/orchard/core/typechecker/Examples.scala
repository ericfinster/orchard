/**
  * Examples.scala - Temporary Examples for Experimentation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import orchard.core.cell._
import orchard.core.util._

import Nats._

trait Examples { thisInterpreter : Interpreter =>

  //============================================================================================
  // SHAPES
  //

  // These can be relativized, I think ....

  def nGlob[A](a : A, sts : (A, A)*) : NCell[A] = 
    if (sts.length <= 0) {
      Object(a)
    } else {
      nGlob(sts.head._1, sts.tail : _*).glob(a, sts.head._2)
    }

  def twoCell[A](top : A, tgt : A, init : A, arrs : (A, A)*) : NCell[A] =  {

    val initialObject = Object(init)

    if (arrs.length <= 0) {
      initialObject.drop(top, tgt)
    } else {

      val firstArrow = Composite(arrs.head._1, Seed(initialObject), arrs.head._1)

      def buildPd(tree : CellTree[_1, A], as : (A, A)*) : CellTree[_1, A] = 
        if (as.length <= 0) {
          tree
        } else {

          val thisArrow = Composite(as.head._1, tree.output.corolla, as.head._2)
          val thisTree = Graft(thisArrow, Vector(tree))

          buildPd(thisTree, as.tail : _*)

        }

      Composite(top, buildPd(firstArrow.corolla, arrs.tail : _*), tgt)

    }
  }

  def obj[A](a : A) : NCell[A] = 
    nGlob(a)

  def arrow[A](arr : A, src : A, tgt : A) : NCell[A] = 
    nGlob(arr, (src, tgt))

  //============================================================================================
  // RAW SYNTAX EXAMPLES
  //

  val identityModule : Module = 
    Module("Identity", Vector(

      Parameter("x", obj(None), false),
      Definition("id-${x}", twoCell(None, None, Some("x")))

    ))

  val compositionModule : Module = 
    Module("Composition", Vector(

      Parameter("x", obj(None), false),
      Parameter("y", obj(None), false),
      Parameter("z", obj(None), false),
      Parameter("f", arrow(None, Some("x"), Some("y")), false),
      Parameter("g", arrow(None, Some("y"), Some("z")), false),

      Definition("${g} o ${f}", 
        twoCell(
          None,
          None,
          Some("x"),
          (Some("f"), Some("y")),
          (Some("g"), Some("z"))
        )
      )

    ))

  // It would be nice to do horizontal composition as well ....

}
