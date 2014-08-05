/**
  * Examples.scala - Type Checker Examples
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.free

import scala.language.implicitConversions

import orchard.core.cell._

trait Examples { thisChecker : TypeChecker =>

  def id : FreeM[Filler] = 
    for {
      _ <- beginModule("Identity")
      x <-   parameter(Identifier("x"), Object(Empty), false)
      i <-   definition(Identifier("id-", x), Object(Full(x)).drop(Empty, Empty))
      _ <- endModule
    } yield i

  def comp : FreeM[Filler] =
    for {
      _ <- beginModule("Composition")
      x <-   parameter(Identifier("x"), Object(Empty), false)
      y <-   parameter(Identifier("y"), Object(Empty), false)
      z <-   parameter(Identifier("z"), Object(Empty), false)
      f <-   parameter(Identifier("f"), Object(Full(x)).glob(Full(y), Empty), false)
      g <-   parameter(Identifier("g"), Object(Full(y)).glob(Full(z), Empty), false)
      c <-   definition(
               Identifier("(", g, " o ", f, ")"),
               simplex(Full(x), Full(y), Full(z), Full(f), Full(g), Empty, Empty)
             )
      _ <- endModule
    } yield c

  def simplex[A](x : A, y : A, z : A, f : A, g : A, h : A, a : A) : NCell[A] = {
    val xPoint = Object(x)
    val fArrow = Composite(f, Seed(xPoint), y)
    val gArrow = Composite(g, fArrow.target.corolla, z)
    Composite(a, Graft(gArrow, Vector(fArrow.corolla)), h)
  }
  

  def rootModule = Module(new ModuleNode(LocalName("root"), Vector.empty), Vector.empty)

  def runExample[A](exmpl : FreeM[A]) : Unit = 
    check(exmpl)(ModuleZipper(rootModule, Nil)) match {
      case Left(msg) => println("Error: " ++ msg)
      case Right(a) => println("Success: " ++ a.toString)
    }


  implicit def stringIsLiteral(str : String) : IdentifierToken = 
    LiteralToken(str)

  implicit def parameterIsReference(p : Parameter) : IdentifierToken = 
    ReferenceToken(p.node.qualifiedName.toString)

  implicit def parameterIsVariable(p : Parameter) : Variable =
    p.parameterNode.variable

  implicit def definitionIsFiller(d : Definition) : Filler =
    d.definitionNode.filler

  implicit def definitionIsReference(d : Definition) : IdentifierToken =
    ReferenceToken(d.node.qualifiedName.toString)

}
