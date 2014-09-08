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

  // object Pt {
  //   def apply[A](a : A) : Tree[_0, A] = Point(a)
  // }

  // implicitly[IsSucc[_3]]
  // implicitly[IsSucc[_1]]

  // object L {
  //   // Ah, okay, make this one an implicit pred ....
  //   def apply[N <: Nat](implicit hp : IsSucc[N]) : Tree[N, Nothing] = {
  //     type P[+X] = Tree[hp.P, X]
  //     hp.leibniz.subst[({ type L[N <: Nat] = Tree[N, Nothing] })#L](Cap[P]())
  //   }
  // }

  // object B {
  //   def apply[N <: Nat, A, B <: A](b : B, shell : Tree[N, Tree[S[N], A]]) : Tree[S[N], A] = {
  //     type P[+X] = Tree[N, X]
  //     Joint[P, A](b, shell)
  //   }
  // }

  //  [ _0, 
  //   Tree[_2, Tree[_3, Nothing]],
  //   Tree[_2, Nothing],
  //   Tree[_1, Tree[_2, Tree[_3, Nothing]]]
  //  ] do not conform to method apply's type parameter bounds [N <: Nat,A,B >: A,T]

  // Right, when you are adding a *tree* as the value at a local branch, and that this is a leaf, it needs to be extended
  // out to be of the right dimension.  How on earth can I do that?

  // Right, it seems actually backwards.  What you want is not to allow the parameter value to be larger, but rather that it is
  // a subtype of the type at the leaves, but possibly not equal to it.  Let's try that ....

  // val list0Raw : Tree[_1, Int] = L[_1]
  // val list1Raw : Tree[_1, Int] = B(0, Pt(L[_1]))
  // val list2Raw : Tree[_1, Int] = B(0, Pt(list1Raw))
  // val list3Raw : Tree[_1, Int] = B(0, Pt(B(1, Pt(L[_1]))))
  // val list4Raw : Tree[_1, Int] = B(0, Pt(B(1, Pt(B(2, Pt(B(3, Pt(L[_1]))))))))

  // val tree0Raw : Tree[_2, Int] = B(1, L[_1])
  // val tree1Raw : Tree[_2, Int] = B(1, B(L[_2], Pt(L[_1])))
  // val tree2Raw : Tree[_2, Int] = B(1, B(L[_2], Pt(B(L[_2], Pt(L[_1])))))

  // val simpleList : Tree[_1, (Int, Int)] = B((7 , 4), Pt(B((6 , 3), Pt(B((5 , 2), Pt(L[_1]))))))

  // val easyTree : Tree[_2, (Int, Int)] = B((13 , 10), B(B((12 , 8), B(L[_2], Pt(B(L[_2], Pt(L[_1]))))), Pt(B(B((11 , 9), B(L[_2], Pt(L[_1]))), Pt(L[_1])))))

  // val fred : Tree[_3, (Int, Int)] = B((25 , 22), B(B((24 , 21), B(L[_3], B(L[_2], Pt(B(B(L[_3], B(B(L[_3], L[_1]), Pt(B(L[_2], Pt(L[_1]))))), Pt(B(L[_2], Pt(L[_1])))))))), B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(B(L[_2], Pt(B(B(B((23 , 20), B(L[_3], B(L[_2], Pt(L[_1])))), B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1]))), Pt(L[_1]))))))))

  // val wilma : Tree[_4, (Int, Int)] = B((41 , 36), B(B((40 , 35), B(L[_4], B(L[_3], B(L[_2], Pt(B(B(B(B((39 , 34), B(B((38 , 33), B(L[_4], B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(B(L[_2], Pt(L[_1]))))))), B(L[_3], B(B(B(L[_4], L[_2]), B(L[_2], Pt(L[_1]))), Pt(B(L[_2], Pt(L[_1]))))))), B(L[_3], B(L[_2], Pt(B(L[_2], Pt(L[_1])))))), B(L[_2], Pt(B(L[_2], Pt(L[_1]))))), Pt(B(B(B(L[_4], B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1])))), B(L[_2], Pt(L[_1]))), Pt(L[_1]))))))))), B(B(L[_4], B(B(L[_4], B(L[_3], B(L[_2], Pt(B(L[_2], Pt(L[_1])))))), B(B(L[_3], B(L[_2], Pt(B(L[_2], Pt(L[_1]))))), Pt(B(L[_2], Pt(L[_1])))))), B(L[_2], Pt(B(B(L[_3], B(L[_2], Pt(B(L[_2], Pt(L[_1]))))), Pt(B(B(B(B((37 , 32), L[_3]), B(L[_3], B(L[_2], Pt(L[_1])))), B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1]))), Pt(L[_1])))))))))

  // val barney : Tree[_6, (Int, Int)] = B((89 , 85), B(L[_6], B(B(L[_6], B(L[_5], B(L[_4], B(L[_3], B(B(L[_3], B(L[_2], Pt(B(B(L[_3], B(L[_2], Pt(B(L[_2], Pt(B(B(L[_3], L[_1]), Pt(L[_1]))))))), Pt(L[_1]))))), Pt(B(B(L[_3], B(B(L[_3], B(B(B(L[_4], L[_2]), B(L[_2], Pt(L[_1]))), Pt(L[_1]))), Pt(L[_1]))), Pt(B(B(L[_3], B(B(L[_3], B(B(L[_3], L[_1]), Pt(L[_1]))), Pt(L[_1]))), Pt(L[_1])))))))))), B(B(B(B((88 , 84), L[_5]), B(B(L[_6], B(L[_5], B(L[_4], B(B(B(L[_5], B(L[_4], B(L[_3], B(B(L[_3], B(L[_2], Pt(B(B(B(L[_4], L[_2]), B(B(L[_3], B(L[_2], Pt(B(L[_2], Pt(B(B(L[_3], L[_1]), Pt(L[_1]))))))), Pt(L[_1]))), Pt(L[_1]))))), Pt(B(L[_2], Pt(B(L[_2], Pt(L[_1]))))))))), B(B(B(L[_5], B(L[_4], B(B(L[_4], B(L[_3], B(L[_2], Pt(B(L[_2], Pt(B(L[_2], Pt(L[_1])))))))), B(L[_2], Pt(B(L[_2], Pt(B(L[_2], Pt(L[_1]))))))))), B(L[_3], B(L[_2], Pt(B(L[_2], Pt(B(L[_2], Pt(L[_1])))))))), B(B(L[_3], B(L[_2], Pt(B(B(L[_3], B(L[_2], Pt(B(L[_2], Pt(B(B(L[_3], L[_1]), Pt(L[_1]))))))), Pt(L[_1]))))), Pt(B(L[_2], Pt(B(L[_2], Pt(L[_1])))))))), B(L[_2], Pt(B(L[_2], Pt(B(L[_2], Pt(B(B(L[_3], B(B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1]))), Pt(L[_1]))), Pt(B(B(B(L[_4], B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1])))), B(B(L[_3], L[_1]), Pt(L[_1]))), Pt(L[_1])))))))))))))), B(L[_4], B(B(L[_4], B(B(L[_4], B(B(L[_4], B(L[_3], B(L[_2], Pt(B(L[_2], Pt(B(L[_2], Pt(L[_1])))))))), B(L[_2], Pt(B(L[_2], Pt(B(L[_2], Pt(L[_1])))))))), B(B(L[_3], B(L[_2], Pt(B(B(B(L[_4], L[_2]), B(B(L[_3], B(L[_2], Pt(B(L[_2], Pt(B(B(L[_3], L[_1]), Pt(L[_1]))))))), Pt(L[_1]))), Pt(L[_1]))))), Pt(B(L[_2], Pt(B(L[_2], Pt(L[_1])))))))), B(L[_2], Pt(B(L[_2], Pt(B(L[_2], Pt(B(B(L[_3], B(B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1]))), Pt(L[_1]))), Pt(B(B(B(L[_4], B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1])))), B(B(L[_3], L[_1]), Pt(L[_1]))), Pt(L[_1])))))))))))))), B(B(L[_5], B(L[_4], B(L[_3], B(L[_2], Pt(B(L[_2], Pt(B(L[_2], Pt(B(B(B(B(B(L[_6], B(B(L[_6], B(L[_5], B(L[_4], B(B(L[_4], B(L[_3], B(L[_2], Pt(L[_1])))), B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1])))))), B(L[_4], B(B(B(L[_5], L[_3]), B(L[_3], B(L[_2], Pt(L[_1])))), B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1])))))), B(L[_4], B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1]))))), B(L[_3], B(B(B(L[_4], B(L[_3], B(L[_2], Pt(L[_1])))), B(L[_2], Pt(L[_1]))), Pt(L[_1])))), B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1]))), Pt(B(B(L[_3], B(B(L[_3], L[_1]), Pt(L[_1]))), Pt(L[_1]))))))))))))), B(B(L[_4], B(B(L[_4], B(B(L[_4], B(L[_3], B(L[_2], Pt(B(L[_2], Pt(B(L[_2], Pt(L[_1])))))))), B(L[_2], Pt(B(L[_2], Pt(B(L[_2], Pt(L[_1])))))))), B(B(L[_3], B(L[_2], Pt(B(B(B(L[_4], L[_2]), B(B(L[_3], B(L[_2], Pt(B(L[_2], Pt(B(B(L[_3], L[_1]), Pt(L[_1]))))))), Pt(L[_1]))), Pt(L[_1]))))), Pt(B(L[_2], Pt(B(L[_2], Pt(L[_1])))))))), B(L[_2], Pt(B(L[_2], Pt(B(L[_2], Pt(B(B(L[_3], B(B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1]))), Pt(L[_1]))), Pt(B(B(B(B(B(B((87 , 83), B(B((86 , 82), B(L[_6], B(L[_5], B(B(L[_5], B(B(B(L[_6], L[_4]), B(L[_4], B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1]))))), B(B(B(L[_5], L[_3]), B(L[_3], B(L[_2], Pt(L[_1])))), B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1]))))), B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1]))))))), B(L[_5], B(B(L[_5], B(L[_4], B(B(B(L[_5], L[_3]), B(L[_3], B(L[_2], Pt(L[_1])))), B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1]))))), B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1]))))))), B(L[_5], B(B(L[_5], B(L[_4], B(B(B(L[_5], L[_3]), B(L[_3], B(L[_2], Pt(L[_1])))), B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1]))))), B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1])))))), B(L[_4], B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1]))))), B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1])))), B(B(L[_3], L[_1]), Pt(L[_1]))), Pt(L[_1]))))))))))))), B(L[_3], B(B(L[_3], B(L[_2], Pt(B(B(L[_3], B(L[_2], Pt(B(L[_2], Pt(B(B(L[_3], L[_1]), Pt(L[_1]))))))), Pt(L[_1]))))), Pt(B(B(L[_3], B(B(L[_3], B(B(B(B(B(L[_6], L[_4]), B(L[_4], L[_2])), L[_2]), B(L[_2], Pt(L[_1]))), Pt(L[_1]))), Pt(L[_1]))), Pt(B(B(L[_3], B(B(L[_3], B(B(L[_3], L[_1]), Pt(L[_1]))), Pt(L[_1]))), Pt(L[_1])))))))))))

  // val dino : Tree[_5, (Int, Int)] = B((22 , 20), B(L[_5], B(L[_4], B(B(B(L[_5], L[_3]), B(L[_3], B(L[_2], Pt(L[_1])))), B(B(B(B(B((21 , 19), L[_4]), B(B(L[_5], L[_3]), B(L[_3], B(L[_2], Pt(L[_1]))))), B(B(L[_4], L[_2]), B(L[_2], Pt(L[_1])))), B(B(B(L[_4], B(L[_3], L[_1])), L[_1]), Pt(L[_1]))), Pt(L[_1]))))))

  // val george : Tree[_4, (Int, Int)] = B((29 , 22), B(L[_4], B(B(B((28 , 21), B(B((27 , 19), B(B((26 , 17), L[_3]), B(B(L[_4], B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1])))), B(L[_2], Pt(L[_1]))))), B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1]))))), B(L[_3], B(B(L[_3], B(L[_2], Pt(L[_1]))), Pt(L[_1])))), B(B(B(B((25 , 20), B(B((24 , 18), B(B((23 , 16), L[_3]), B(B(L[_4], B(L[_3], B(B(L[_3], L[_1]), Pt(L[_1])))), L[_1]))), B(L[_3], B(B(L[_3], L[_1]), Pt(L[_1]))))), B(L[_3], B(B(L[_3], L[_1]), Pt(L[_1])))), L[_1]), Pt(L[_1])))))

  // val jane : Tree[_4, (Int, Int)] = B((15 , 12), B(B((14 , 11), B(B((13 , 10), L[_3]), B(L[_3], L[_1]))), B(B(L[_4], B(L[_3], B(B(L[_3], B(B(L[_3], L[_1]), Pt(L[_1]))), Pt(L[_1])))), L[_1])))

}
