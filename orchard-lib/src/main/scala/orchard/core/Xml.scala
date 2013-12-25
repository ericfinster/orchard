/**
  * Xml.scala - XML Routines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import Nats._

trait XmlSerializable[A] {
  def toXML(a : A) : xml.Node
  def fromXML(node : xml.Node) : A
}

object XmlSerializable {

  // Just for kicks ...
  def trimText(els : Seq[xml.Node]) = els filterNot (_.isInstanceOf[xml.Text])

  implicit object StringSerializable extends XmlSerializable[String] {
    def toXML(str : String) = xml.Text(str)
    def fromXML(node : xml.Node) : String = node.text.trim
  }

  implicit def polaritySerializable[A : XmlSerializable] : XmlSerializable[Polarity[A]] =
    new XmlSerializable[Polarity[A]] {
      val ev = implicitly[XmlSerializable[A]]
      def toXML(p : Polarity[A]) = 
        p match {
          case Positive => <positive />
          case Negative => <negative />
          case Neutral(a) => <neutral>{ev.toXML(a)}</neutral>
        }

      def fromXML(node : xml.Node) : Polarity[A] =
        node match {
          case <positive /> => Positive
          case <negative /> => Negative
          case <neutral>{content}</neutral> => Neutral(ev.fromXML(content))
          case _ => throw new IllegalArgumentException("Polarity read failed.")
        }
    }

  implicit def optionSerializable[A : XmlSerializable] : XmlSerializable[Option[A]] =
    new XmlSerializable[Option[A]] {
      val ev = implicitly[XmlSerializable[A]]
      def toXML(o : Option[A]) = 
        o match {
          case None => <none />
          case Some(a) => <some>{ev.toXML(a)}</some>
        }

      def fromXML(node : xml.Node) : Option[A] =
        node match {
          case <none /> => None
          case <some>{content}</some> => Some(ev.fromXML(content))
          case _ => throw new IllegalArgumentException("Option read failed.")
        }
    }

  implicit def pairSerializable[A : XmlSerializable, B : XmlSerializable] : XmlSerializable[(A, B)] = 
    new XmlSerializable[(A, B)] {
      val aIsSerializable = implicitly[XmlSerializable[A]]
      val bIsSerializable = implicitly[XmlSerializable[B]]
      def toXML(pr : (A, B)) = {
        val (a, b) = pr
        <pair><fst>{aIsSerializable.toXML(a)}</fst><snd>{bIsSerializable.toXML(b)}</snd></pair>
      }
      def fromXML(node : xml.Node) = 
        node match {
          case <pair><fst>{aContent}</fst><snd>{bContent}</snd></pair> => {
            (aIsSerializable.fromXML(aContent), bIsSerializable.fromXML(bContent))
          }
        }
    }

  implicit def roseTreeSerializable[A : XmlSerializable, B : XmlSerializable] : XmlSerializable[RoseTree[A, B]] = 
    new XmlSerializable[RoseTree[A, B]] {
      val aIsSerializable = implicitly[XmlSerializable[A]]
      val bIsSerializable = implicitly[XmlSerializable[B]]
      def toXML(t : RoseTree[A, B]) =
        t match {
          case Rose(b) => <rose>{bIsSerializable.toXML(b)}</rose>
          case Branch(a, brs) => <branch>{aIsSerializable.toXML(a)}<branches>{brs map (b => toXML(b))}</branches></branch>
        }
      def fromXML(node : xml.Node) =
        node match {
          case <rose>{content}</rose> => Rose(bIsSerializable.fromXML(content))
          case <branch>{aContent}<branches>{brsContent @ _*}</branches></branch> => {
            val a = aIsSerializable.fromXML(aContent)
            val brs = trimText(brsContent).toVector map (br => fromXML(br))
            Branch(a, brs)
          }
        }
    }

  // You should probably look at this again.  It seems like there is a more
  // efficient way ... Yeah, this is wildly inefficient.  Fix it!!
  implicit val expressionSerializable : XmlSerializable[Expression] =
    new XmlSerializable[Expression] {
      def toXML(expr : Expression) = 
        expr match {
          case Variable(id, isThin) => <variable id={id} isThin={isThin.toString} />
          case Filler(id, nook) => 
            <filler id={id}>{pairSerializable[RoseTree[Option[String], Option[String]], Option[String]].toXML(nook)}</filler>
          case FillerTarget(id, nook, isThin) =>
            <fillertgt id={id} isThin={isThin.toString}>{pairSerializable[RoseTree[Option[String], Option[String]], Option[String]].toXML(nook)}</fillertgt>
        }

      def fromXML(node : xml.Node) =
        node match {
          case v @ <variable /> => {
            val id = (v \ "@id").text
            val isThin = (v \ "@isThin").text.toBoolean
            Variable(id, isThin)
          }
          case f @ <filler>{content}</filler> => {
            val id = (f \ "@id").text
            val nook = pairSerializable[RoseTree[Option[String], Option[String]], Option[String]].fromXML(content)
            Filler(id, nook)
          }
          case ft @ <fillertgt>{content}</fillertgt> => {
            val id = (ft \ "@id").text
            val isThin = (ft \ "@isThin").text.toBoolean
            val nook = pairSerializable[RoseTree[Option[String], Option[String]], Option[String]].fromXML(content)
            FillerTarget(id, nook, isThin)
          }
        }
    }

  implicit def cellSerializable[A : XmlSerializable] : XmlSerializable[Cell[_ <: Nat, A]] =
    new XmlSerializable[Cell[_ <: Nat, A]] {
      val ev = implicitly[XmlSerializable[A]]

      def toXML(cell : Cell[_ <: Nat, A]) : xml.Node = {
        val cplx = new SimpleMutableComplex(cell)
        val nodes = new ListBuffer[xml.Node]

        cplx forAllCells (c => nodes += c.cellToXML)

        <complex topCell={cplx.topCell.hashCode.toString}>{nodes}</complex>
      }

      def fromXML(node : xml.Node) : Cell[_ <: Nat, A] = {
        node match {
          case <complex>{cells @ _*}</complex> => {
            val topCellId = (node \ "@topCell").text.toInt
            val env = new HashMap[Int, Cell[_ <: Nat, A]]

            for (cell <- trimText(cells)) {
              cell match {
                case <obj>{objContents @ _*}</obj> => {
                  val id = (cell \ "@id").text.toInt
                  val item = ev.fromXML(trimText((objContents \\ "label")(0).descendant)(0))

                  env(id) = Object(item)
                }
                case <cell>{cellContents @ _*}</cell> => {

                  val id = (cell \ "@id").text.toInt
                  val targetId = ((cellContents \\ "target")(0) \ "@ref").text.toInt
                  val item = ev.fromXML(trimText((cellContents \\ "label")(0).descendant)(0))

                  def extractTree[D <: Nat](node : xml.Node) : CellTree[D, A] = {
                    node match {
                      case s @ <seed /> => {
                        val srcId = (s \ "@ref").text.toInt
                        val srcObj = env(srcId).asInstanceOf[ObjectCell[_0, A]]

                        Seed(srcObj).asInstanceOf[CellTree[D, A]]
                      }
                      case l @ <leaf /> => {
                        val leafId = (l \ "@ref").text.toInt
                        val leafCell = env(leafId).asInstanceOf[Cell[D#Pred, A]]

                        Leaf(leafCell).asInstanceOf[CellTree[D, A]]
                      }
                      case g @ <graft>{graftContents @ _*}</graft> => {
                        // Fake it.
                        implicit val hasPred : HasPred[D] = new HasPred[D] { type Pred = D#Pred }

                        val graftId = (g \ "@ref").text.toInt
                        val graftCell = env(graftId).asInstanceOf[Cell[D, A]]
                        val branches = trimText(graftContents).toVector map (n => extractTree[D](n))

                        Graft(graftCell, branches)
                      }
                      case other @ _ => throw new IllegalArgumentException
                    }
                  }

                  val dim : Nat = env(targetId).dimension
                  val srcTree = extractTree[dim.Self](trimText((cellContents \\ "sourcetree")(0).descendant)(0))

                  env(id) = Composite(item, srcTree, env(targetId).value)
                }
                case other @ _ => println("Skipping unrecognized element." ++ other.toString)
              }
            }

            env(topCellId)
          }
          case _ => throw new IllegalArgumentException("Not a complex")
        }
      }
    }
}
