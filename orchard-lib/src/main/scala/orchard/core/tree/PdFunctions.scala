/**
  * PdFunctions.scala - Functions on pasting diagrams
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds

import Nats._
import Trees._
import PastingDiagrams._

trait PdFunctions[N <: Nat] {

  implicit val tfns : TreeFunctions[N]

  def succ : PdFunctions[S[N]]

  def plug[A](d : DerivativePd[N, A], a : A) : Pd[N, A] = 
    close(d._2, Box(a, d._1))

  def close[A](c : ContextPd[N, A], pd : Pd[N, A]) : Pd[N, A] =
    c match {
      case Nil => pd
      case ((a , d) :: cs) => 
        close(cs, Box(a, tfns.plug(d, pd)))
    }

  def visit[A](addr : Address[N], z : ZipperPd[N, A]) : Option[ZipperPd[N, A]] 
  def sibling[A](addr : Address[N], z : ZipperPd[S[N], A]) : Option[ZipperPd[S[N], A]]

  def seek[A](addr : Address[S[N]], z : ZipperPd[N, A]) : Option[ZipperPd[N, A]] = 
    addr match {
      case Nil => Some(z)
      case d :: ds => 
        for {
          zz <- seek(ds, z)
          zzz <- visit(d, z)
        } yield zzz
    }

  def fromXml[A](node : xml.Node)(implicit xmlRead : XmlReadable[A]) : Pd[N, A]

}

object PdZeroFunctions extends PdFunctions[_0] { self =>

  implicit val tfns : TreeFunctions[_0] = TreeZeroFunctions

  def succ : PdFunctions[_1] = 
    new PdSuccFunctions[_0] {
      implicit val tfns = self.tfns.succ
      val prev = self
    }

  def visit[A](addr : Address[_0], z : ZipperPd[_0, A]) : Option[ZipperPd[_0, A]] =
    z match {
      case (Obj(_), cntxt) => None
      case (Box(h, Point(t)), cntxt) => Some((t , (h, ()) :: cntxt))
    }

  def sibling[A](addr : Address[_0], z : ZipperPd[_1, A]) : Option[ZipperPd[_1, A]] = 
    z match {
      case (_, Nil) => None
      case (fcs, (a , (Point(Leaf(ad)) , hcn)) :: cn) => None
      case (fcs, (a , (Point(Node(nfcs, shell)), hcn)) :: cn) => 
        Some((nfcs , (a , (shell , ((fcs , ()) :: hcn))) :: cn))
    }

  def fromXml[A](node : xml.Node)(implicit xmlRead : XmlReadable[A]) : Pd[_0, A] =
    node match {
      case <obj>{lblContent}</obj> => Obj(xmlRead.read(lblContent))
      case <box><label>{lblContent}</label><shell>{shellContent}</shell></box> => {

        implicit val pdReader = new XmlReadable[Pd[_0, A]] {
          def read(nd : xml.Node) = fromXml(nd)
        }

        val a : A = xmlRead.read(lblContent)
        val sh : Tree[_0, Pd[_0, A]] = tfns.fromXml(shellContent)(pdReader)

        Box(a, sh)
      }
    }

}

trait PdSuccFunctions[N <: Nat] extends PdFunctions[S[N]] { self => 

  val prev : PdFunctions[N]

  def succ : PdFunctions[S[S[N]]] = 
    new PdSuccFunctions[S[N]] {
      implicit val tfns = self.tfns.succ
      val prev = self
    }

  // Err.  This could be better ...
  val prevTfns : TreeFunctions[N] = prev.tfns

  // def test[A](tr : Tree[N, A]) : TreeOps[N, A] = tr
  // def blorp[A](tr : Tree[N, A]) : Option[A] = test(tr).rootValue
  // def bleep[A](tr : Tree[N, A]) : Option[A] = tr.rootValue

  def visit[A](addr : Address[S[N]], z : ZipperPd[S[N], A]) : Option[ZipperPd[S[N], A]] =
    z match {
      case (Dot(_, _), cntxt) => None
      case (Box(a, shell), cntxt) =>
        for {
          zz <- tfns.seek(addr, (shell, Nil))
          res <- zz._1 match {
            case Leaf(_) => None
            case Node(pd, hsh) => Some((pd, (a, (hsh, zz._2)) :: cntxt))
          }

        } yield res
    }

  def sibling[A](addr : Address[S[N]], z : ZipperPd[S[S[N]], A]) : Option[ZipperPd[S[S[N]], A]] =
    z match {
      case (_, Nil) => None
      case (fcs, (a , (verts, hcn)) :: cn) => ???
        for {
          zz <- tfns.seek(addr, (verts, Nil))
        } yield ???  // Arrrgghh!! Can't seem to do deep matching ....

        // pdSibling {suc n} dir (fcs , (a , verts , hcn) ∷ cn) =
        //   seek dir (verts , [])
        //   >>= (λ { (leaf l , vcn) → nothing ;
        //            (node (leaf l) shell , vcn) → nothing ;
        //            (node (node nfcs vrem) hmask , vcn) →
        //              just (nfcs , (a , (vrem , (fcs , (hmask , vcn)) ∷ hcn)) ∷ cn) })

    }

  def fromXml[A](node : xml.Node)(implicit xmlRead : XmlReadable[A]) : Pd[S[N], A] =
    node match {
      case <dot><label>{lblContent}</label><corolla>{corollaContent}</corolla></dot> => {

        import XmlReadable.addressReadable

        val a : A = xmlRead.read(lblContent)
        val corolla = prevTfns.fromXml(corollaContent)(addressReadable(prevTfns.dim))

        Dot(a, corolla)

      }
      case <box><label>{lblContent}</label><shell>{shellContent}</shell></box> => {

        implicit val pdReader = new XmlReadable[Pd[S[N], A]] {
          def read(nd : xml.Node) = fromXml(nd)
        }

        val a : A = xmlRead.read(lblContent)
        val sh : Tree[S[N], Pd[S[N], A]] = tfns.fromXml(shellContent)(pdReader)

        Box(a, sh)
      }
    }

}
