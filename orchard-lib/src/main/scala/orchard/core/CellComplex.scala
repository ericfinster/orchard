/**
  * CellComplex.scala - A Cell Complex
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core

import scala.collection.mutable.ListBuffer

import Util._

trait CellComplex[A] extends EventEmitter[CellEvent] { thisComplex =>

  type CellType <: ComplexCell

  def newCell(item : A) : CellType

  def baseCells : List[CellType] 

  def topCell : CellType = baseCells.last
  def dimension : Int = baseCells.length - 1
  def apply(idx : Int) : CellType = baseCells(idx)
  def toCell : NCell[A] = topCell.skeleton map (_.item)

  def forAllCells(action : CellType => Unit) : Unit = {
    baseCells foreach (base => base foreachCell action)
  }

  //============================================================================================
  // COMPLEX CELLS
  //

  trait ComplexCell 
      extends CellBase[CellType, CellType] 
      with    EdgeBase[CellType, CellType] 
      with    EventEmitter[CellEvent] { thisCell : CellType =>

    def item : A
    def complex : CellComplex[A] = thisComplex

    var loops : List[CellType] = Nil
    var skeleton : NCell[CellType] = null

    //============================================================================================
    // EVENT EMISSION
    //

    def emitToFaces(ev : CellEvent) : Unit =
      skeleton map (face => { face.emit(ev) })

    def emitToNeighborhood(ev : CellEvent) : Unit =
      topCell.skeleton map (face => { if (face.hasFace(this)) face.emit(ev) })

    //============================================================================================
    // SEMANTIC ROUTINES
    //

    def neighborhood : List[CellType] = {
      val neighbors = new ListBuffer[CellType]
      topCell.skeleton map (face => { if (face.hasFace(this)) { neighbors += face } })
      neighbors.toList
    }

    def hasFace(cell : CellType) : Boolean = {
      var found : Boolean = false
      skeleton map (face => { found = found | (face == cell) })
      found
    }

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
            baseContainer.incoming.force
          }

        next.topCell
      }
    }

    //============================================================================================
    // XML CONVERSION
    //

    def cellToXML(implicit vs : XmlSerializable[A]) = 
      skeleton.cell match {
        case Object(_, ev) => <obj id={hashCode.toString}><label>{vs.toXML(item)}</label></obj>
        case Composite(_, srcTree, tgtValue, ev) => {
          <cell id={hashCode.toString}><sourcetree>{

            def processSourceTree(tree : CellTree[_ <: Nat, CellType]) : xml.NodeSeq = 
              tree match {
                case Seed(o, _) => <seed ref={o.value.hashCode.toString} />
                case Leaf(l, _) => <leaf ref={l.value.hashCode.toString} />
                case Graft(c, brs, _) => <graft ref={c.value.hashCode.toString}>{ brs map processSourceTree }</graft>
              }

            processSourceTree(srcTree)
          }</sourcetree><target ref={tgtValue.hashCode.toString} /><label>{vs.toXML(item)}</label></cell>
        }
      }
  }

  //============================================================================================
  // COMPLEX GENERATION
  //

  object ComplexGenerator extends CellRegenerator[A, CellType] {

    def generateObject[D <: Nat : IsZero](value : A) : Cell[D, CellType] = {
      val newObj = newCell(value)
      val newObjSkeleton = ObjectCell(newObj)
      newObj.skeleton = newObjSkeleton
      newObjSkeleton
    }

    def generateCell[D <: Nat : HasPred](cellValue : A,
                                         srcs : CellTree[D#Pred, CellType],
                                         tgtValue : A) : Cell[D, CellType] = {

      val thisMutableCell = newCell(cellValue)
      val tgtMutableCell = newCell(tgtValue)

      thisMutableCell.shell = None
      thisMutableCell.target = Some(tgtMutableCell)
      thisMutableCell.sources = Some(srcs.cellList map
                                       (src => {
                                          src.value.outgoing = Some(thisMutableCell)
                                          src.value
                                        }))

      // We know the incoming cells
      thisMutableCell.incoming = None
      tgtMutableCell.incoming = Some(thisMutableCell)

      var curIdx : Int = -1

      def processSources(srcTree : CellTree[D#Pred, CellType])
          : (RoseTree[CellType, Int],
             Option[CellType],
             Option[List[CellType]]) =
        srcTree match {
          case Seed(obj, ev) =>
            {
              obj.value.container = Some(tgtMutableCell)
              (Branch(obj.value, Rose(curIdx) :: Nil), None, None)
            }
          case Leaf(shape, ev) =>
            {
              curIdx += 1
              (Rose(curIdx), Some(shape.value), Some(shape.value :: Nil))
            }
          case Graft(cell, branches, ev) =>
            {
              implicit val hasPred = ev

              val (newBranches, tgtOpts, srcOpts) = (branches map (b => processSources(b))).unzip3
              val srcs = for { ll <- optSwitch(srcOpts) } yield ll.flatten
              cell.value.container = Some(tgtMutableCell)
              (Branch(cell.value, newBranches), Some(cell.targetValue), srcs)
            }
        }

      val (tgtShell, tgtTgtOpt, tgtSrcsOpt) = processSources(srcs)

      tgtMutableCell.shell = Some(tgtShell)
      tgtMutableCell.target = tgtTgtOpt
      tgtMutableCell.sources = tgtSrcsOpt

      if (tgtMutableCell.isLoop) {
        for { t <- tgtMutableCell.target } {
          t.loops = tgtMutableCell :: t.loops
        }
      }

      val thisMutableCellSkeleton = CompositeCell(thisMutableCell, srcs, tgtMutableCell)
      thisMutableCell.skeleton = thisMutableCellSkeleton
      tgtMutableCell.skeleton = thisMutableCellSkeleton.target
      thisMutableCellSkeleton
    }
  }
}


