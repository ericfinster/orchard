/**
  * Examples.scala - Examples of the new tree library
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions

import Nats._
import Trees._

object TreeExamples {

  val fred : Tree[_3, (Int, Int)] = Node((25 , 22), Node(Node((24 , 21), Node(Leaf[_3], Node(Leaf[_2], Pt(Node(Node(Leaf[_3], Node(Node(Leaf[_3], Leaf[_1]), Pt(Node(Leaf[_2], Pt(Leaf[_1]))))), Pt(Node(Leaf[_2], Pt(Leaf[_1])))))))), Node(Node(Leaf[_3], Node(Leaf[_2], Pt(Leaf[_1]))), Pt(Node(Leaf[_2], Pt(Node(Node(Node((23 , 20), Node(Leaf[_3], Node(Leaf[_2], Pt(Leaf[_1])))), Node(Node(Leaf[_3], Node(Leaf[_2], Pt(Leaf[_1]))), Pt(Leaf[_1]))), Pt(Leaf[_1]))))))))

  val fredT1 : Tree[_2, (Int, Int)] = Node((18 , 13), Node(Node((19 , 12), Node(Leaf[_2], Pt(Leaf[_1]))), Pt(Node(Node((17 , 11), Node(Node((16 , 10), Leaf[_1]), Pt(Node(Leaf[_2], Pt(Leaf[_1]))))), Pt(Node(Node((15 , 9), Node(Node((14 , 8), Node(Leaf[_2], Pt(Leaf[_1]))), Pt(Leaf[_1]))), Pt(Leaf[_1])))))))

  val fredT2 : Tree[_1, (Int, Int)] = Node((7 , 4), Pt(Node((6 , 3), Pt(Node((5 , 2), Pt(Leaf[_1]))))))

  val fredT3 : Tree[_0, Int] = Pt(1)

}
