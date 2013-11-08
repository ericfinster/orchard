/**
  * Examples.scala - Example Opetopes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.language.implicitConversions

import Nats._

object Example {
  val x = Object("x")

  val f = Composite("f", Seed(x), "y")
  val g = Composite("g", f.target.corolla, "z")
  val h = Composite("h", g.target.corolla, "w")

  val beta = Composite("beta", Leaf(g.target), "j")
  val gamma = Composite("gamma", Graft(h, Graft(beta.target, g.corolla :: Nil) :: Nil), "k")
  val alpha = Composite("alpha", f.corolla, "i")
  val delta = Composite("delta", Graft(gamma.target, alpha.target.corolla :: Nil), "l")

  val Z = Composite("Z", Graft(gamma, Leaf(g) :: beta.corolla :: Leaf(h) :: Nil), "eta")
  val X = Composite("X", Leaf(alpha.target), "epsilon")
  val Y = Composite("Y", Graft(X.target, alpha.corolla :: Nil), "zeta")
  val W = Composite("W", Graft(delta, Y.target.corolla :: Z.target.corolla :: Nil), "theta")

  val Psi = Composite("Psi", Graft(W, Graft(Y, Leaf(alpha) :: X.corolla :: Nil) :: Z.corolla :: Leaf(delta) :: Nil), "S")
}

// [info] <cell label="x" id="1969931593">  </cell>
// [info] <cell label="f" id="1403886703"> <source id="1969931593"/> </cell>
// [info] <cell label="g" id="143241509"> <source id="130873476"/> </cell>
// [info] <cell label="h" id="567256604"> <source id="1201834395"/> </cell>
// [info] <cell label="-" id="2146976730"> <source id="70180569"/> </cell>
// [info] <cell label="beta" id="556300980">  </cell>
// [info] <cell label="gamma" id="1754255469"> <source id="143241509"/><source id="968677820"/><source id="567256604"/> </cell>
// [info] <cell label="alpha" id="494592833"> <source id="1403886703"/> </cell>
// [info] <cell label="delta" id="1860030341"> <source id="610491808"/><source id="2006435337"/> </cell>
// [info] <cell label="-" id="223000118"> <source id="604019211"/><source id="2146976730"/> </cell>
// [info] <cell label="Z" id="595679908"> <source id="556300980"/><source id="1754255469"/> </cell>
// [info] <cell label="X" id="23451442">  </cell>
// [info] <cell label="Y" id="1158878293"> <source id="494592833"/><source id="1738275808"/> </cell>
// [info] <cell label="W" id="833971268"> <source id="2076820954"/><source id="2078059187"/><source id="1860030341"/> </cell>
// [info] <cell label="-" id="1414204748"> <source id="190194640"/><source id="223000118"/> </cell>
// [info] <cell label="Psi" id="1145647744"> <source id="23451442"/><source id="1158878293"/><source id="595679908"/><source id="833971268"/> </cell>
// [info] <cell label="-" id="563254406"> <source id="508885666"/><source id="1414204748"/> </cell>
// [info] <cell label="-" id="1561506108"> <source id="1145647744"/><source id="563254406"/> </cell>

object Example1 {
  val a = Object(1)

  val b = Composite(8, Seed(a), 2)
  val c = Composite(9, b.target.corolla, 3)
  val d = Composite(12, c.target.corolla, 4)
  val e = Composite(14, d.target.corolla, 5)
  val f = Composite(15, e.target.corolla, 6)
  val g = Composite(16, f.target.corolla, 7)

  val h = Composite(24, Leaf(c.target), 10)
  val i = Composite(22, Graft(h.target, Graft(c, b.corolla :: Nil) ::  Nil), 11)
  val j = Composite(27, d.corolla, 13)
  val k = Composite(29, Graft(g, f.corolla :: Nil), 17)
  val l = Composite(19, Graft(k.target, Graft(e, Graft(j.target, i.target.corolla :: Nil) :: Nil) :: Nil), 18)

  val m = Composite(36, Leaf(c), 23)
  val n = Composite(35, Graft(i, Leaf(b) :: m.target.corolla :: h.corolla :: Nil), 25)
  val o = Composite(37, Leaf(i.target), 26)
  val p = Composite(38, Leaf(f), 32)
  val q = Composite(40, Leaf(f), 30)
  val r = Composite(39, Graft(k, q.target.corolla :: Leaf(g) :: Nil), 31)
  val s = Composite(41, Leaf(k.target), 28)
  val t = Composite(43, Leaf(l.target), 20)
  val u = Composite(42, Graft(t.target, l.corolla :: Nil), 21)
  val v = Composite(34, Graft(u.target,
                              Graft(o.target, n.target.corolla :: Nil) :: j.corolla :: Leaf(e) :: Graft(s.target, Graft(r.target, p.target.corolla :: Leaf(g) :: Nil) :: Nil) :: Nil), 33)

  val w = Composite(45, Graft(v,
                              Graft(n, m.corolla :: Leaf(h) :: Leaf(i) :: Nil) ::
                                o.corolla ::
                                Leaf(j) ::
                                p.corolla ::
                                Graft(r, q.corolla :: Leaf(k) :: Nil) ::
                                s.corolla ::
                                Graft(u, Leaf(l) :: t.corolla :: Nil) :: Nil), 44)
}
