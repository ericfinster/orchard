/**
  * Util.scala - Utility objects and classes
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import Nats._

// trait XmlSerializable[A] {
//   def toXML(a : A) : xml.Node
//   def fromXML(node : xml.Node) : A
// }

// object XmlSerializable {

//   // Just for kicks ...
//   def trimText(els : Seq[xml.Node]) = els filterNot (_.isInstanceOf[xml.Text])

//   implicit object StringSerializable extends XmlSerializable[String] {
//     def toXML(str : String) = xml.Text(str)
//     def fromXML(node : xml.Node) : String = node.text.trim
//   }

//   implicit def polaritySerializable[A : XmlSerializable] : XmlSerializable[Polarity[A]] =
//     new XmlSerializable[Polarity[A]] {
//       val ev = implicitly[XmlSerializable[A]]
//       def toXML(p : Polarity[A]) = 
//         p match {
//           case Positive => <positive />
//           case Negative => <negative />
//           case Neutral(a) => <neutral>{ev.toXML(a)}</neutral>
//         }

//       def fromXML(node : xml.Node) : Polarity[A] =
//         node match {
//           case <positive /> => Positive
//           case <negative /> => Negative
//           case <neutral>{content}</neutral> => Neutral(ev.fromXML(content))
//           case _ => throw new IllegalArgumentException("Polarity read failed.")
//         }
//     }

//   implicit def optionSerializable[A : XmlSerializable] : XmlSerializable[Option[A]] =
//     new XmlSerializable[Option[A]] {
//       val ev = implicitly[XmlSerializable[A]]
//       def toXML(o : Option[A]) = 
//         o match {
//           case None => <none />
//           case Some(a) => <some>{ev.toXML(a)}</some>
//         }

//       def fromXML(node : xml.Node) : Option[A] =
//         node match {
//           case <none /> => None
//           case <some>{content}</some> => Some(ev.fromXML(content))
//           case _ => throw new IllegalArgumentException("Option read failed.")
//         }
//     }

//   // You should probably look at this again.  It seems like there is a more
//   // efficient way ... Yeah, this is wildly inefficient.  Fix it!!
//   implicit def expressionSerializable : XmlSerializable[Expression] =
//     new XmlSerializable[Expression] {
//       def toXML(expr : Expression) = 
//         expr match {
//           case Variable(id, isThin) => <variable id={id} isThin={isThin.toString} />
//           case Filler(id, nook) => 
//             <filler id={id}>{cellSerializable[Option[Expression]].toXML(nook)}</filler>
//           case FillerTarget(id, nook, isThin) =>
//             <fillertgt id={id} isThin={isThin.toString}>{cellSerializable[Option[Expression]].toXML(nook)}</fillertgt>
//         }

//       def fromXML(node : xml.Node) =
//         node match {
//           case v @ <variable /> => {
//             val id = (v \ "@id").text
//             val isThin = (v \ "@isThin").text.toBoolean
//             Variable(id, isThin)
//           }
//           case f @ <filler>{content}</filler> => {
//             val id = (f \ "@id").text
//             val nook = cellSerializable[Option[Expression]].fromXML(content)
//             Filler(id, nook)
//           }
//           case ft @ <fillertgt>{content}</fillertgt> => {
//             val id = (ft \ "@id").text
//             val isThin = (ft \ "@isThin").text.toBoolean
//             val nook = cellSerializable[Option[Expression]].fromXML(content)
//             FillerTarget(id, nook, isThin)
//           }
//         }
//     }

//   implicit def cellSerializable[A : XmlSerializable] : XmlSerializable[Cell[_ <: Nat, A]] =
//     new XmlSerializable[Cell[_ <: Nat, A]] {
//       val ev = implicitly[XmlSerializable[A]]

//       def toXML(cell : Cell[_ <: Nat, A]) : xml.Node = {
//         val cplx = new SimpleMutableComplex(cell)
//         val nodes = new ListBuffer[xml.Node]

//         cplx forAllCells (c => nodes += c.cellToXML)

//         <complex topCell={cplx.topCell.hashCode.toString}>{nodes}</complex>
//       }

//       def fromXML(node : xml.Node) : Cell[_ <: Nat, A] = {
//         node match {
//           case <complex>{cells @ _*}</complex> => {
//             val topCellId = (node \ "@topCell").text.toInt
//             val env = new HashMap[Int, Cell[_ <: Nat, A]]

//             for (cell <- trimText(cells)) {
//               cell match {
//                 case <obj>{objContents @ _*}</obj> => {
//                   val id = (cell \ "@id").text.toInt
//                   val item = ev.fromXML(trimText((objContents \\ "label")(0).descendant)(0))

//                   env(id) = Object(item)
//                 }
//                 case <cell>{cellContents @ _*}</cell> => {

//                   val id = (cell \ "@id").text.toInt
//                   val targetId = ((cellContents \\ "target")(0) \ "@ref").text.toInt
//                   val item = ev.fromXML(trimText((cellContents \\ "label")(0).descendant)(0))

//                   def extractTree[D <: Nat](node : xml.Node) : CellTree[D, A] = {
//                     node match {
//                       case s @ <seed /> => {
//                         val srcId = (s \ "@ref").text.toInt
//                         val srcObj = env(srcId).asInstanceOf[ObjectCell[_0, A]]

//                         Seed(srcObj).asInstanceOf[CellTree[D, A]]
//                       }
//                       case l @ <leaf /> => {
//                         val leafId = (l \ "@ref").text.toInt
//                         val leafCell = env(leafId).asInstanceOf[Cell[D#Pred, A]]

//                         Leaf(leafCell).asInstanceOf[CellTree[D, A]]
//                       }
//                       case g @ <graft>{graftContents @ _*}</graft> => {
//                         // Fake it.
//                         implicit val hasPred : HasPred[D] = new HasPred[D] { type Pred = D#Pred }

//                         val graftId = (g \ "@ref").text.toInt
//                         val graftCell = env(graftId).asInstanceOf[Cell[D, A]]
//                         val branches = trimText(graftContents).toList map (n => extractTree[D](n))

//                         Graft(graftCell, branches)
//                       }
//                       case other @ _ => throw new IllegalArgumentException
//                     }
//                   }

//                   val dim : Nat = env(targetId).dimension
//                   val srcTree = extractTree[dim.Self](trimText((cellContents \\ "sourcetree")(0).descendant)(0))

//                   env(id) = Composite(item, srcTree, env(targetId).value)
//                 }
//                 case other @ _ => println("Skipping unrecognized element." ++ other.toString)
//               }
//             }

//             env(topCellId)
//           }
//           case _ => throw new IllegalArgumentException("Not a complex")
//         }
//       }
//     }
// }

object Util {
  // Please find a better place to put this ... or a library version?
  implicit class Force[A](opt : Option[A]) {
    def force : A =
      opt match {
        case None => throw new IllegalArgumentException("Force failed.")
        case Some(a) => a
      }

    def force(msg : String) : A =
      opt match {
        case None => throw new IllegalArgumentException(msg)
        case Some(a) => a
      }
  }

  def optSwitch[X](opts : List[Option[X]]) : Option[List[X]] =
    opts match {
      case Nil => Some(Nil)
      case o :: os => for { n <- o ; ns <- optSwitch(os) } yield (n :: ns)
    }

  def optSwitchVect[X](opts : Vector[Option[X]]) : Option[Vector[X]] = {
    try {
      Some(opts map (_.get))
    } catch {
      case e : NoSuchElementException => None
    }
  }

  var debug : Boolean = false
}
