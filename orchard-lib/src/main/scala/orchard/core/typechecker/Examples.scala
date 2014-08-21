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

  val prelude : Module =
    Module("Prelude", Vector(

      identityModule,
      compositionModule,

      Module("Composition Test", Vector(

        Parameter("a", obj(None), false),
        Parameter("b", obj(None), false),
        Parameter("c", obj(None), false),
        Parameter("s", arrow(None, Some("a"), Some("b")), false),
        Parameter("t", arrow(None, Some("b"), Some("c")), false),

        Import("Composition Import", "Composition", Object(None), Map(
          "f" -> "s",
          "g" -> "t"
        ))

      ))

    ))


  //============================================================================================
  // TRAVERSAL EXAMPLES
  //

  import scalaz._
  import State._

  import scalaz.syntax.traverse._

  type IntState[A] = State[Int, A]

  def incState : State[Int, Unit] = 
    for {
      i <- get
      _ <- put(i + 1)
    } yield ()

  val stateTest : NCell[State[Int, Unit]] = 
    arrow(incState, incState, incState)

  val stateComp : State[Int, NCell[Unit]] = 
    stateTest.sequence[IntState, Unit]

  val result : Int = stateComp.exec(0)

}
