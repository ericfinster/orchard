/**
  * TreeIndex.scala - Indexing of Tree-related types
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.cell

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.Id._

sealed trait TreeIndex {

  type Self <: TreeIndex

  type Tree[+_]
  type Context[+_]
  type Derivative[+_]
  type Zipper[+_]

  type Direction
  type Address = List[Direction]

}

case object ZT extends TreeIndex {

  type Self = this.type

  type Tree[+A] = Id[A]
  type Context[+A] = Unit
  type Derivative[+A] = Unit
  type Zipper[+A] = (Unit, Unit)

  type Direction = Nothing

}

case class ST[Pred <: TreeIndex](pred : Pred) extends TreeIndex {

  type Self = ST[Pred]

  type Tree[+A] = Slice[Pred#Tree, A]
  type Context[+A] = List[(A, Pred#Derivative[Tree[A]])]
  type Derivative[+A] = (Pred#Tree[Tree[A]], Context[A])
  type Zipper[+A] = (Tree[A], Context[A])

  type Direction = List[Pred#Direction]

}


object TreeIndex {

  type _0 = ZT.type
  type _1 = ST[_0]
  type _2 = ST[_1]
  type _3 = ST[_2]

  object IsZeroIndex {
    def unapply[N <: TreeIndex](n : N) : Option[ZeroImplicits[N]] =
      n match {
        case ZT => Some(new ZeroImplicits[N] { })
        case _ => None
      }
  }

  object IsOneIndex {
    def unapply[N <: TreeIndex](n : N) : Option[OneImplicits[N]] = 
      n match {
        case ST(ZT) => Some(new OneImplicits[N] { })
        case _ => None
      }
  }

  object IsSuccIndex {
    def unapply[N <: TreeIndex](n : N) : Option[SuccImplicits[N]] = 
      n match {
        case ST(pred) => 
          Some(new SuccImplicits[N] { type P = pred.Self ; val p = pred.asInstanceOf[P] })
        case _ => None
      }
  }

  object IsDblSuccIndex {
    def unapply[N <: TreeIndex](n : N) : Option[GteTwoImplicits[N]] = 
      n match {
        case ST(ST(ppred)) =>
          Some(new GteTwoImplicits[N] { type PP = ppred.Self ; val pp = ppred.asInstanceOf[PP] })
        case _ => None
      }
  }

  trait ZeroImplicits[N <: TreeIndex] {

    implicit def treeCoh[A](zt : ZT.Tree[A]) : N#Tree[A] = zt.asInstanceOf[N#Tree[A]]
    implicit def contextCoh[A](zc : ZT.Context[A]) : N#Context[A] = zc.asInstanceOf[N#Context[A]]
    implicit def derivCoh[A](zd : ZT.Derivative[A]) : N#Derivative[A] = zd.asInstanceOf[N#Derivative[A]]
    implicit def zipCoh[A](zz : ZT.Zipper[A]) : N#Zipper[A] = zz.asInstanceOf[N#Zipper[A]]
    implicit def dirCoh(zd : ZT.Direction) : N#Direction = zd.asInstanceOf[N#Direction]
    implicit def addrCoh(za : ZT.Address) : N#Address = za.asInstanceOf[N#Address]

    implicit def treeCoe[A](nt : N#Tree[A]) : ZT.Tree[A] = nt.asInstanceOf[ZT.Tree[A]]
    implicit def contextCoe[A](nc : N#Context[A]) : ZT.Context[A] = nc.asInstanceOf[ZT.Context[A]]
    implicit def derivCoe[A](nd : N#Derivative[A]) : ZT.Derivative[A] = nd.asInstanceOf[ZT.Derivative[A]]
    implicit def zipCoe[A](nz : N#Derivative[A]) : ZT.Zipper[A] = nz.asInstanceOf[ZT.Zipper[A]]
    implicit def dirCoe(nd : N#Direction) : ZT.Direction = nd.asInstanceOf[ZT.Direction]
    implicit def addCoe[A](na : N#Address) : ZT.Address = na.asInstanceOf[ZT.Address]

  }

  trait OneImplicits[N <: TreeIndex] {

    implicit def treeCoh[A](zt : ST[ZT.type]#Tree[A]) : N#Tree[A] = zt.asInstanceOf[N#Tree[A]]
    implicit def contextCoh[A](zc : ST[ZT.type]#Context[A]) : N#Context[A] = zc.asInstanceOf[N#Context[A]]
    implicit def derivCoh[A](zd : ST[ZT.type]#Derivative[A]) : N#Derivative[A] = zd.asInstanceOf[N#Derivative[A]]
    implicit def zipCoh[A](zz : ST[ZT.type]#Zipper[A]) : N#Zipper[A] = zz.asInstanceOf[N#Zipper[A]]
    implicit def dirCoh(zd : ST[ZT.type]#Direction) : N#Direction = zd.asInstanceOf[N#Direction]
    implicit def addrCoh(za : ST[ZT.type]#Address) : N#Address = za.asInstanceOf[N#Address]

    implicit def treeCoe[A](nt : N#Tree[A]) : ST[ZT.type]#Tree[A] = nt.asInstanceOf[ST[ZT.type]#Tree[A]]
    implicit def contextCoe[A](nc : N#Context[A]) : ST[ZT.type]#Context[A] = nc.asInstanceOf[ST[ZT.type]#Context[A]]
    implicit def derivCoe[A](nd : N#Derivative[A]) : ST[ZT.type]#Derivative[A] = nd.asInstanceOf[ST[ZT.type]#Derivative[A]]
    implicit def zipCoe[A](nz : N#Zipper[A]) : ST[ZT.type]#Zipper[A] = nz.asInstanceOf[ST[ZT.type]#Zipper[A]]
    implicit def dirCoe(nd : N#Direction) : ST[ZT.type]#Direction = nd.asInstanceOf[ST[ZT.type]#Direction]
    implicit def addCoe[A](na : N#Address) : ST[ZT.type]#Address = na.asInstanceOf[ST[ZT.type]#Address]

  }

  trait SuccImplicits[N <: TreeIndex] {

    type P <: TreeIndex

    val p : P

    type PTree[+A] = P#Tree[A]

    type STree[+A] = ST[P]#Tree[A]
    type SDeriv[+A] = ST[P]#Derivative[A]

    implicit def treeCoh[A](st : ST[P]#Tree[A]) : N#Tree[A] = st.asInstanceOf[N#Tree[A]]
    implicit def contextCoh[A](sc : ST[P]#Context[A]) : N#Context[A] = sc.asInstanceOf[N#Context[A]]
    implicit def derivCoh[A](sd : ST[P]#Derivative[A]) : N#Derivative[A] = sd.asInstanceOf[N#Derivative[A]]
    implicit def zipCoh[A](sz : ST[P]#Zipper[A]) : N#Zipper[A] = sz.asInstanceOf[N#Zipper[A]]
    implicit def dirCoh(sd : ST[P]#Direction) : N#Direction = sd.asInstanceOf[N#Direction]
    implicit def addrCoh(sa : ST[P]#Address) : N#Address = sa.asInstanceOf[N#Address]

    implicit def treeCoe[A](nt : N#Tree[A]) : ST[P]#Tree[A] = nt.asInstanceOf[ST[P]#Tree[A]]
    implicit def contextCoe[A](nc : N#Context[A]) : ST[P]#Context[A] = nc.asInstanceOf[ST[P]#Context[A]]
    implicit def derivCoe[A](nd : N#Derivative[A]) : ST[P]#Derivative[A] = nd.asInstanceOf[ST[P]#Derivative[A]]
    implicit def zipCoe[A](nz : N#Derivative[A]) : ST[P]#Zipper[A] = nz.asInstanceOf[ST[P]#Zipper[A]]
    implicit def dirCoe(nd : N#Direction) : ST[P]#Direction = nd.asInstanceOf[ST[P]#Direction]
    implicit def addrCoe(na : N#Address) : ST[P]#Address = na.asInstanceOf[ST[P]#Address]

  }

  trait GteTwoImplicits[N <: TreeIndex] {

    type PP <: TreeIndex
    type P = ST[PP]

    type FS[+A] = Slice[PP#Tree, A]

    val pp : PP
    val p : P = ST(pp)

    implicit def treeCoh[A](st : ST[ST[PP]]#Tree[A]) : N#Tree[A] = st.asInstanceOf[N#Tree[A]]
    implicit def contextCoh[A](sc : ST[ST[PP]]#Context[A]) : N#Context[A] = sc.asInstanceOf[N#Context[A]]
    implicit def derivCoh[A](sd : ST[ST[PP]]#Derivative[A]) : N#Derivative[A] = sd.asInstanceOf[N#Derivative[A]]
    implicit def zipCoh[A](sz : ST[ST[PP]]#Zipper[A]) : N#Zipper[A] = sz.asInstanceOf[N#Zipper[A]]
    implicit def dirCoh(sd : ST[ST[PP]]#Direction) : N#Direction = sd.asInstanceOf[N#Direction]
    implicit def addrCoh(sa : ST[ST[PP]]#Address) : N#Address = sa.asInstanceOf[N#Address]

    implicit def treeCoe[A](nt : N#Tree[A]) : ST[ST[PP]]#Tree[A] = nt.asInstanceOf[ST[ST[PP]]#Tree[A]]
    implicit def contextCoe[A](nc : N#Context[A]) : ST[ST[PP]]#Context[A] = nc.asInstanceOf[ST[ST[PP]]#Context[A]]
    implicit def derivCoe[A](nd : N#Derivative[A]) : ST[ST[PP]]#Derivative[A] = nd.asInstanceOf[ST[ST[PP]]#Derivative[A]]
    implicit def zipCoe[A](nz : N#Zipper[A]) : ST[ST[PP]]#Zipper[A] = nz.asInstanceOf[ST[ST[PP]]#Zipper[A]]
    implicit def dirCoe(nd : N#Direction) : ST[ST[PP]]#Direction = nd.asInstanceOf[ST[ST[PP]]#Direction]
    implicit def addrCoe(na : N#Address) : ST[ST[PP]]#Address = na.asInstanceOf[ST[ST[PP]]#Address]

  }

}
