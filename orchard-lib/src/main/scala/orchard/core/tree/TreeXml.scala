/**
  * TreeXml.scala - Xml Serialization and Deserialization for trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import Nats._
import Trees._

trait XmlReadable[A] {

  def read(node : xml.Node) : A

}

object XmlReadable {

  implicit def intReadable : XmlReadable[Int] = 
    new XmlReadable[Int] {
      def read(node : xml.Node) = node.text.toInt
    }

  implicit def addressReadable[N <: Nat](implicit n : N) : XmlReadable[Address[N]] = 
    n match {
      case IsZero(zm) => 
        new XmlReadable[Address[N]] {
          def read(node : xml.Node) : Address[N] = 
            node match {
              case <unit/> => Nil
            }
        }
      case IsSucc(sm) => { import sm._ ;
        new XmlReadable[Address[N]] {
          def read(node : xml.Node) : Address[N] = 
            node match {
              case <nil/> => Nil
              case <cons><hd>{hdContent}</hd><tl>{tlContent}</tl></cons> => {

                val d : Address[P] = addressReadable(p).read(hdContent)
                val ds : Address[S[P]] = succCoh.subst[Address](read(tlContent))

                succCoe.subst[Address](d :: ds)
              }
            }
        }
      }
    }

  implicit def treeReadable[N <: Nat, A](
    implicit aRd : XmlReadable[A], 
    tfns : TreeFunctions[N]
  ) : XmlReadable[Tree[N, A]] =
    new XmlReadable[Tree[N, A]] {
      def read(node : xml.Node) = tfns.fromXml(node)(aRd)
    }

}


trait XmlWritable[A] {

  def write(a : A) : xml.Node

}




