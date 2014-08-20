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

  def globOn(x : ExpressionEntry, globName : String, tgtName : String) : FreeM[Parameter] = 
    for {
      shape <- shapeOf(x.expression)
      asEntry = shape map (Full(_))
      target <- parameter(Identifier(tgtName), asEntry.glob(Empty, Empty).target, false)
      glob <- parameter(Identifier(globName), asEntry.glob(Empty, Full(target)), false)
    } yield glob

  def nGlob(top : String, faces : (String, String)*) : FreeM[Parameter] = 
    if (faces.length == 0) {
      obj(top)
    } else {
      for {
        prev <- nGlob(faces.head._1, faces.tail : _*)
        next <- globOn(prev, top, faces.head._2)
      } yield next
    }

  def obj(name : String) : FreeM[Parameter] = 
    for {
      o <- parameter(Identifier(name), Object(Empty), false)
    } yield o

  def arrow(src : String, tgt : String, arr : String) : FreeM[Parameter] = 
    nGlob(arr, (src, tgt))

  def twoCell(
    twoCell : String, 
    srcArrow : String, tgtArrow : String, 
    srcObj : String, tgtObj : String
  ) : FreeM[Parameter] =
    nGlob(twoCell, (srcArrow, tgtArrow), (srcObj, tgtObj))

  def example = twoCell("a", "f", "g", "x", "y")

  def id(x : ExpressionEntry) : FreeM[Definition] =
    for {
      w <- worksheetWithExpression(x.expression)
      _ <- selectAsBase(w, initialObject)
      d <- drop(w)
      nook <- extract(w, d.filler)
      i <- definition(Identifier("id-", x), nook)
    } yield i

  def compose(f : ExpressionEntry, g : ExpressionEntry) : FreeM[Definition] = 
    for {
      w <- worksheetWithExpression(f.expression)
      t <- targetAddress(w, initialObject)
      _ <- selectAsBase(w, t)
      e <- extrude(w)
      _ <- paste(w, e.filler, g.expression)
      _ <- selectAsBase(w, e.filler)
      _ <- selectRay(w, Vector(0))
      s <- extrude(w)
      nook <- extract(w, s.filler)
      d <- definition(Identifier("(", g, " o ", f, ")"), nook)
    } yield d

  def testCompose : FreeM[Definition] = 
    for {
      f <- arrow("x", "y", "f")
      g <- arrow("y", "z", "g")
      composite <- compose(f, g)
      _ = println(composite.definitionNode.filler.toString)
    } yield composite

  def identityTest : FreeM[Definition] =
    for {
      composite <- testCompose
      idOfComp <- id(composite)
      _ = println(idOfComp.definitionNode.filler.toString)
    } yield idOfComp

  def myTest : FreeM[Definition] = 
    for {
      blah <- example
      otherBlah <- id(blah)
    } yield otherBlah

  def makeDrop : FreeM[CellAddress] = 
    for {
      w <- emptyWorksheet
      _ <- selectAsBase(w, initialObject)
      dropResult <- drop(w)
      (fillerAddr, targetAddr) = dropResult
    } yield fillerAddr

  def makeGlob : FreeM[CellAddress] = 
    for {
      w <- emptyWorksheet
      _ <- selectAsBase(w, initialObject)
      extrudeResult <- extrude(w)
      (fillerAddr, targetAddr) = extrudeResult
      _ <- selectAsBase(w, fillerAddr)
      extrudeResult2 <- extrude(w)
    } yield extrudeResult2._1

  def makeSimplex : FreeM[CellAddress] = 
    for {
      w <- emptyWorksheet
      _ <- selectAsBase(w, initialObject)
      e0 <- extrude(w)
      _ <- selectAsBase(w, e0.target)
      e1 <- extrude(w)
      _ <- selectAsBase(w, e1.filler)
      _ <- selectTo(w, e0.filler)
      simplexExtrusion <- extrude(w)
    } yield simplexExtrusion.filler

  def horizontalCompositeExtrusion : FreeM[Unit] = 
    for {
      w <- emptyWorksheet
      _ <- selectAsBase(w, initialObject)
      f <- extrude(w)
      _ <- selectAsBase(w, f.target)
      g <- extrude(w)
      _ <- selectAsBase(w, f.filler)
      a <- extrude(w)
      _ <- selectAsBase(w, g.filler)
      b <- extrude(w)
    } yield ()

  def simplex[A](x : A, y : A, z : A, f : A, g : A, h : A, a : A) : NCell[A] = {
    val xPoint = Object(x)
    val fArrow = Composite(f, Seed(xPoint), y)
    val gArrow = Composite(g, fArrow.target.corolla, z)
    Composite(a, Graft(gArrow, Vector(fArrow.corolla)), h)
  }
  
  def initialObject : CellAddress = 
    Source(Immediate, 0 :: Nil)

  def rootModule = Module(new ModuleNode(LocalName("root"), Vector.empty), Vector.empty)

  def runExample[A](exmpl : FreeM[A]) : Unit = 
    check(exmpl)(ModuleZipper(rootModule, Nil)) match {
      case Left(msg) => println("Error: " ++ msg)
      case Right(a) => println("Success: " ++ a.toString)
    }

  def logState : FreeM[Unit] = {
    for {
      s <- examineState
    } yield dumpState(s)
  }

  def dumpLocalScope : FreeM[Unit] = 
    for {
      s <- examineLocalScope
    } yield {

      println("Dumping local scope: ")

      for {
        (qualName, entry) <- s
      } {
        println(qualName ++ " -> " ++ entry.toString)
      }

    }

  def dumpModuleScope : FreeM[Unit] = 
    for {
      s <- examineModuleScope
    } yield {

      println("Dumping module scope: ")

      for {
        (qualName, entry) <- s
      } {
        println(qualName ++ " -> " ++ entry.toString)
      }

    }

  def dumpState(cursor : ModuleZipper) : Unit = {
    println("Focus is at: " ++ cursor.focus.node.toString)

    cursor.focus match {
      case m : Module => {
        println("Active module: " ++ m.node.toString)

        for {
          entry <- m.entries
        } {
          println("Entry: " ++ entry.node.toString)
        }
      }
      case _ => ()
    }
  }

  implicit def stringIsLiteral(str : String) : IdentifierToken = 
    LiteralToken(str)

  implicit def expressionEntryIsReference(e : ExpressionEntry) : IdentifierToken = 
    ReferenceToken(e.node.qualifiedName.toString)

  implicit def parameterIsVariable(p : Parameter) : Variable =
    p.parameterNode.variable

  implicit def definitionIsFiller(d : Definition) : Filler =
    d.definitionNode.filler

  implicit class ExtrusionResult(addr : (CellAddress, CellAddress)) {

    def filler : CellAddress = addr._1
    def target : CellAddress = addr._2

  }

}
