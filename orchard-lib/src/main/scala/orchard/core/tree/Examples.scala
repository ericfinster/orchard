/**
  * Examples.scala - Examples of the new tree library
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import Nats._
import Trees._

object TreeExamples {

  // implicitly[IsTree[Tree[_1, Int], _1, Int]]

  // val ex0 : Tree[_1, Int] = cons(0, cons(1, cons(2, nil)))
  // val ex0F : Option[Tree0[Unit]] = TreeLib.flatten(Z, ex0)

  // val myTree : Tree[_2, Int] = 
  //   node(3, cons(leaf, nil))

  // val fred : Tree[_3, Int] = joint(25, node(joint(24, node(cap, cons(leaf, cons(node(cap, cons(leaf, cons(node(cap, nil), nil))), cons(leaf, nil))))), cons(node(joint(23, node(cap, cons(leaf, nil))), cons(node(cap, cons(leaf, nil)), nil)), cons(leaf, cons(node(cap, cons(leaf, nil)), nil)))))

  // val flattenFred : Option[Tree2[Unit]] = fred.flatten

  // val pd15 : Tree3[Int] = joint(15, node(joint(14, leaf), cons(node(cap, cons(leaf, cons(leaf, nil))), nil)))


}
