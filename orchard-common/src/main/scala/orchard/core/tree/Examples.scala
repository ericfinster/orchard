/**
  * Examples.scala - Repository for generated examples
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import Nats._
import Tree._
import Suite._
import Complex._

object NestingExamples {

  val fred0 : Nesting[_0, Int] = Box(4, Pt(Box(3, Pt(Box(2, Pt(Obj(1)))))))

  val fred1 : Nesting[_1, Int] = Box(13, Node(Box(12, Node(Dot(7, Pt(Root())),Pt(Leaf(Root())))),Pt(Node(Box(11, Node(Box(10, Leaf(Root())),Pt(Node(Dot(6, Pt(Root())),Pt(Leaf(Root())))))),Pt(Node(Box(9, Node(Box(8, Node(Dot(5, Pt(Root())),Pt(Leaf(Root())))),Pt(Leaf(Root())))),Pt(Leaf(Root()))))))))

  val fred2 : Nesting[_2, Int] = Box(22, Node(Box(21, Node(Dot(18, Node(Root(),Pt(Node(Step(Dir(Root()), Root()),Pt(Node(Step(Dir(Root()), Step(Dir(Root()), Root())),Pt(Leaf(Root())))))))),Node(Leaf(Root()),Pt(Node(Node(Dot(17, Node(Root(),Pt(Node(Step(Dir(Root()), Root()),Pt(Leaf(Root())))))),Node(Node(Dot(16, Leaf(Root())),Leaf(Root())),Pt(Node(Leaf(Step(Dir(Root()), Root())),Pt(Leaf(Root())))))),Pt(Node(Leaf(Step(Dir(Root()), Step(Dir(Root()), Root()))),Pt(Leaf(Root()))))))))),Node(Node(Dot(19, Node(Root(),Pt(Leaf(Root())))),Node(Leaf(Root()),Pt(Leaf(Root())))),Pt(Node(Leaf(Step(Dir(Root()), Root())),Pt(Node(Node(Box(20, Node(Dot(15, Node(Root(),Pt(Leaf(Root())))),Node(Leaf(Root()),Pt(Leaf(Root()))))),Node(Node(Dot(14, Node(Root(),Pt(Leaf(Root())))),Node(Leaf(Step(Dir(Root()), Step(Dir(Root()), Root()))),Pt(Leaf(Root())))),Pt(Leaf(Root())))),Pt(Leaf(Root())))))))))

  val fred3 : Nesting[_3, Int] = Box(26, Node(Dot(25, Node(Root(),Node(Node(Step(Dir(Root()), Root()),Node(Leaf(Root()),Pt(Leaf(Root())))),Pt(Node(Leaf(Step(Dir(Root()), Root())),Pt(Node(Node(Step(Dir(Step(Dir(Root()), Step(Dir(Root()), Root()))), Root()),Node(Node(Step(Dir(Root()), Step(Dir(Step(Dir(Root()), Step(Dir(Root()), Root()))), Root())),Node(Leaf(Step(Dir(Root()), Step(Dir(Root()), Root()))),Pt(Leaf(Root())))),Pt(Leaf(Root())))),Pt(Leaf(Root()))))))))),Node(Node(Dot(24, Node(Root(),Node(Leaf(Root()),Pt(Node(Node(Step(Dir(Step(Dir(Root()), Root())), Root()),Node(Node(Step(Dir(Root()), Step(Dir(Step(Dir(Root()), Root())), Root())),Leaf(Root())),Pt(Node(Leaf(Step(Dir(Root()), Root())),Pt(Leaf(Root())))))),Pt(Node(Leaf(Step(Dir(Root()), Step(Dir(Root()), Root()))),Pt(Leaf(Root()))))))))),Node(Leaf(Root()),Node(Leaf(Root()),Pt(Node(Node(Leaf(Step(Dir(Step(Dir(Root()), Root())), Root())),Node(Node(Leaf(Step(Dir(Root()), Step(Dir(Step(Dir(Root()), Root())), Root()))),Leaf(Root())),Pt(Node(Leaf(Step(Dir(Root()), Root())),Pt(Leaf(Root())))))),Pt(Node(Leaf(Step(Dir(Root()), Step(Dir(Root()), Root()))),Pt(Leaf(Root()))))))))),Node(Node(Leaf(Step(Dir(Root()), Root())),Node(Leaf(Root()),Pt(Leaf(Root())))),Pt(Node(Leaf(Step(Dir(Root()), Root())),Pt(Node(Node(Node(Dot(23, Node(Root(),Node(Leaf(Root()),Pt(Leaf(Root()))))),Node(Leaf(Step(Dir(Step(Dir(Root()), Step(Dir(Root()), Root()))), Root())),Node(Leaf(Root()),Pt(Leaf(Root()))))),Node(Node(Leaf(Step(Dir(Root()), Step(Dir(Step(Dir(Root()), Step(Dir(Root()), Root()))), Root()))),Node(Leaf(Step(Dir(Root()), Step(Dir(Root()), Root()))),Pt(Leaf(Root())))),Pt(Leaf(Root())))),Pt(Leaf(Root()))))))))))

  val fred4 : Nesting[_4, Int] = Dot(27, Node(Root(),Node(Node(Step(Dir(Root()), Root()),Node(Leaf(Root()),Node(Leaf(Root()),Pt(Node(Node(Leaf(Step(Dir(Step(Dir(Root()), Root())), Root())),Node(Node(Leaf(Step(Dir(Root()), Step(Dir(Step(Dir(Root()), Root())), Root()))),Leaf(Root())),Pt(Node(Leaf(Step(Dir(Root()), Root())),Pt(Leaf(Root())))))),Pt(Node(Leaf(Step(Dir(Root()), Step(Dir(Root()), Root()))),Pt(Leaf(Root()))))))))),Node(Node(Leaf(Step(Dir(Root()), Root())),Node(Leaf(Root()),Pt(Leaf(Root())))),Pt(Node(Leaf(Step(Dir(Root()), Root())),Pt(Node(Node(Node(Step(Dir(Step(Dir(Step(Dir(Root()), Step(Dir(Root()), Root()))), Root())), Root()),Node(Leaf(Step(Dir(Step(Dir(Root()), Step(Dir(Root()), Root()))), Root())),Node(Leaf(Root()),Pt(Leaf(Root()))))),Node(Node(Leaf(Step(Dir(Root()), Step(Dir(Step(Dir(Root()), Step(Dir(Root()), Root()))), Root()))),Node(Leaf(Step(Dir(Root()), Step(Dir(Root()), Root()))),Pt(Leaf(Root())))),Pt(Leaf(Root())))),Pt(Leaf(Root()))))))))))

  val fred : Complex[_4, Int] =  fred0 >> fred1 >> fred2 >> fred3 >> fred4

}
