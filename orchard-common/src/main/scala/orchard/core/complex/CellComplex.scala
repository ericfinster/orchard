/**
  * CellComplex.scala - A Cell Complex
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core.complex

import orchard.core.cell._
import orchard.core.util._

import Util._

trait CellComplex[A] { thisComplex =>

  type CellType <: ComplexCell

  def newCell(item : A) : CellType

  def topCell : CellType
  def baseCells : List[CellType] = topCell.targets
  def dimension : Int = baseCells.length - 1
  def apply(idx : Int) : CellType = baseCells(idx)

  def forAllCells(action : CellType => Unit) : Unit = {
    baseCells foreach (base => base foreachCell action)
  }

  def forAllCells(startDim : Int, endDim : Int, action : CellType => Unit) = {
    baseCells.slice(startDim, endDim) foreach (base => base foreachCell action)
  }

  def foreachProperFace(action : CellType => Unit) : Unit = {
    forAllCells(0, dimension, action)
  }

  def forAllFaces(action : CellType => Unit) : Unit = 
    if (dimension > 0)
      baseCells(dimension - 1) foreachCell action

  def seek(addr : CellAddress) : Option[CellType] = 
    addr match {
      case Immediate => Some(topCell)
      case Target(prefix) => 
        for { 
          pref <- seek(prefix) 
          tgt <- pref.target 
        } yield tgt
      // case Source(Immediate, loc) => 
      //   Some((topCell.sources.get)(loc.head))
      case Source(prefix, loc) =>
        for { 
          pref <- seek(prefix) 
          ptr <- new RoseZipper(pref.totalCanopy, Nil).seek(loc)
        } yield {
          ptr.focus match {
            case Rose(idx) => (pref.sources.get)(idx) 
            case Branch(cell, _) => cell.target.get
          }
        }
    }

  //============================================================================================
  // COMPLEX CELLS
  //

  trait ComplexCell 
      extends MutableCellBase[CellType, CellType] 
      with    MutableEdgeBase[CellType, CellType] { thisCell : CellType =>

    def item : A

    def complex : CellComplex[A] = thisComplex

    // That these are mutable here is dubious ...
    var loops : List[CellType] = Nil

    //============================================================================================
    // SEMANTIC ROUTINES
    //

    def faces : List[CellType]
    def neighborhood : List[CellType]

    def properTargets : List[CellType] =
      target match {
        case None => Nil
        case Some(tgt) => tgt :: tgt.properTargets
      }

    def dimension : Int = properTargets.length

    def targets : List[CellType] =
      (this :: properTargets).reverse

    def isTopCell : Boolean = (isExternal && isBase && loops == Nil)
    def isArrow : Boolean =
      target match {
        case None => false
        case Some(cell) => cell.isObject
      }

    def topCell : CellType = {
      if (this.isTopCell) this else {
        val next =
          if (loops != Nil) {
            loops.head
          } else {
            baseContainer.incoming.get
          }

        next.topCell
      }
    }

    def extractCanopy(verticalBoundary : Vector[CellType]) : RoseTree[CellType, Int] = {

      def verticalTrace(cell : CellType, lvs : Vector[RoseTree[CellType, Int]]) : RoseTree[CellType, Int] = {
        if ((verticalBoundary contains cell) || cell.isExternal) {
          Branch(cell, lvs)
        } else {

          def horizontalTrace(tr : RoseTree[CellType, Int]) : RoseTree[CellType, Int] =
            tr match {
              case Rose(idx) => if (cell.isObject) { Rose(idx) } else lvs(idx)
              case Branch(hCell, hBranches) => {
                verticalTrace(hCell, hBranches map horizontalTrace)
              }
            }

          horizontalTrace(cell.canopy.get)
        }
      }

      val startLeaves = Range(0, sourceCount) map (i => Rose(i))
      verticalTrace(thisCell, startLeaves.toVector)

    }

    def totalCanopy : RoseTree[CellType, Int] =
      extractCanopy(Vector.empty)

    def sourceTree : Option[RoseTree[CellType, Int]] = 
      for {
        tgt <- target
        srcs <- sources
      } yield {
        tgt.extractCanopy(srcs)
      }

    def address : CellAddress = {
      if (thisCell.dimension == thisComplex.dimension) {
        Immediate
      } else if (thisCell.dimension == thisComplex.dimension - 1) {
        if (! isExternal) {
          Target(Immediate)
        } else {
          Source(Immediate, List(topCell.sources.get indexOf thisCell))
        }
      } else {

        def buildPrefix(i : Int) : AddressPrefix =
          if (i <= 0) Immediate else Target(buildPrefix(i - 1))

        if (isBase) {
          buildPrefix(thisComplex.dimension - thisCell.dimension)
        } else {
          // This should be the generic case.  We look at the base cell two dimensions higher,
          // and get it's source tree.  Then we look for this guy as an edge in that rose tree

          val refCell = baseCells(thisCell.dimension + 2)
          val refSrcs = baseCells(thisCell.dimension + 1).sources.get

          val ptr = new RoseZipper(refCell.sourceTree.get, Nil).find(
            branchCell => {
              branchCell.target == Some(thisCell)
            },
            roseIdx => {
              refSrcs(roseIdx) == thisCell
            }).get


          val prefix : AddressPrefix =
            buildPrefix(thisComplex.dimension - thisCell.dimension - 1)

          Source(prefix, ptr.toAddr)
        }
      }
    }
  }

  //============================================================================================
  // JSON SERIALIZATION ROUTINES
  //

  abstract class CellDescriptor {

    def item : A

    def canopy : Option[RoseTree[Int, Int]]
    def sources : Option[Vector[Int]]
    def target : Option[Int]
    def container : Option[Int]
    def incoming : Option[Int]
    def outgoing : Option[Int]
    def loops : List[Int]
    def faces : List[Int]

  }

  def cellToDescriptor(cell : CellType) : CellDescriptor =
    new CellDescriptor {

      def item = cell.item

      def canopy = cell.canopy map (_.map((_.hashCode), (i => i)))
      def sources = cell.sources map (_.map(_.hashCode))
      def target = cell.target map (_.hashCode)
      def container = cell.container map (_.hashCode)
      def incoming = cell.incoming map (_.hashCode)
      def outgoing = cell.outgoing map (_.hashCode)
      def loops = cell.loops map (_.hashCode)
      def faces = cell.faces map (_.hashCode)

    }

  def cellFromDescriptor(cell : CellType, desc : CellDescriptor, cellMap : Map[Int, CellType]) : Unit = {

    cell.canopy = desc.canopy map (_ map ((cellMap(_)), (i => i)))
    cell.sources = desc.sources map (_ map (cellMap(_)))
    cell.target = desc.target map (cellMap(_))
    cell.container = desc.container map (cellMap(_))
    cell.incoming = desc.incoming map (cellMap(_))
    cell.outgoing = desc.outgoing map (cellMap(_))
    cell.loops = desc.loops map (cellMap(_))

  }

  def descriptorToJson[P](
    desc : CellDescriptor,
    writer : JsonWriter[P],
    aWriter : JsonWritable[A, P]
  ) : P = {

    val canopyWriter = implicitly[JsonWritable[Option[RoseTree[Int, Int]], P]]
    val sourceWriter = implicitly[JsonWritable[Option[Array[Int]], P]]
    val optionWriter = implicitly[JsonWritable[Option[Int], P]]
    val loopWriter = implicitly[JsonWritable[Array[Int], P]]

    writer.writeObject(
      ("canopy" -> canopyWriter.write(desc.canopy, writer)),
      ("target" -> optionWriter.write(desc.target, writer)),
      ("sources" -> sourceWriter.write(desc.sources map (_.toArray), writer)),
      ("container" -> optionWriter.write(desc.container, writer)),
      ("incoming" -> optionWriter.write(desc.incoming, writer)),
      ("outgoing" -> optionWriter.write(desc.outgoing, writer)),
      ("loops" -> loopWriter.write(desc.loops.toArray, writer)),
      ("faces" -> loopWriter.write(desc.faces.toArray, writer)),
      ("item" -> aWriter.write(desc.item, writer))
    )

  }

  def descriptorFromJson[P](
    x : P, 
    reader : JsonReader[P],
    aReader : JsonReadable[A, P]
  ) : CellDescriptor = {

    val canopyReader = implicitly[JsonReadable[Option[RoseTree[Int, Int]], P]]
    val sourceReader = implicitly[JsonReadable[Option[Vector[Int]], P]]
    val optionReader = implicitly[JsonReadable[Option[Int], P]]
    val loopReader = implicitly[JsonReadable[Vector[Int], P]]

    val canopyObj = reader.readObjectField(x, "canopy")
    val sourceObj = reader.readObjectField(x, "sources")
    val targetObj = reader.readObjectField(x, "target")
    val containerObj = reader.readObjectField(x, "container")
    val incomingObj = reader.readObjectField(x, "incoming")
    val outgoingObj = reader.readObjectField(x, "outgoing")
    val loopsObj = reader.readObjectField(x, "loops")
    val facesObj = reader.readObjectField(x, "faces")
    val itemObj = reader.readObjectField(x, "item")

    new CellDescriptor {

      def item = aReader.read(itemObj, reader)

      def canopy = canopyReader.read(canopyObj, reader)
      def sources = sourceReader.read(sourceObj, reader)
      def target = optionReader.read(targetObj, reader)
      def container = optionReader.read(containerObj, reader)
      def incoming = optionReader.read(incomingObj, reader)
      def outgoing = optionReader.read(outgoingObj, reader)
      def loops = loopReader.read(loopsObj, reader).toList
      def faces = loopReader.read(facesObj, reader).toList

    }

  }

  // Right, so this is the idea, but I think we can do it with many
  // fewer traversals and much more efficiently.  Need to look if we
  // can loop over the fields of an object in ScalaJS ...

  def toJson[P](
    writer : JsonWriter[P],
    aWriter : JsonWritable[A, P]
  ) : P = {
    import scala.collection.mutable.ListBuffer
    val fields : ListBuffer[(String, P)] = ListBuffer.empty

    forAllCells (cell => {
      fields += (cell.hashCode.toString -> 
        descriptorToJson(
          cellToDescriptor(cell),
          writer, aWriter
        )
      )
    })

    val idArray : ListBuffer[P] = 
      (fields map { case (id, _) => writer.writeString(id) })

    // We need to see if we can loop over objects, because this
    // here is a bit inefficient ...
    writer.writeObject(
      ("complex" -> writer.writeDouble(hashCode)),
      ("ids" -> writer.writeArray(idArray : _*)),
      ("cells" -> writer.writeObject(fields : _*)),
      ("topCell" -> writer.writeDouble(topCell.hashCode))
    )
  }

  def fromJson[P](
    x : P,
    reader : JsonReader[P],
    aReader : JsonReadable[A, P]
  ) : CellType = {

    val idArrayReader = implicitly[JsonReadable[Array[String], P]]
    val idArrayObj = reader.readObjectField(x, "ids")
    val idArray = idArrayReader.read(idArrayObj, reader)

    val cellObj = reader.readObjectField(x, "cells")

    val descMap : Map[Int, (CellDescriptor, CellType)] =
      Map(
        idArray map (id => {
          val desc = 
            descriptorFromJson(
              reader.readObjectField(cellObj, id),
              reader, aReader
            )

          (id.toInt -> (desc, newCell(desc.item)))
        }) : _*
      )

    val newCellMap = descMap mapValues {
      case (_, c) => c
    }

    descMap foreach {
      case (id, (desc, cell)) => {
        cellFromDescriptor(cell, desc, newCellMap)
      }
    }

    val topCellId = reader.readNumber(reader.readObjectField(x, "topCell"))

    newCellMap(topCellId.toInt)
  }
}

