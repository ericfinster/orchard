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
  val gamma = Composite("gamma", Graft(h, Vector(Graft(beta.target, Vector(g.corolla)))), "k")
  val alpha = Composite("alpha", f.corolla, "i")
  val delta = Composite("delta", Graft(gamma.target, Vector(alpha.target.corolla)), "l")

  val Z = Composite("Z", Graft(gamma, Vector(Leaf(g), beta.corolla, Leaf(h))), "eta")
  val X = Composite("X", Leaf(alpha.target), "epsilon")
  val Y = Composite("Y", Graft(X.target, Vector(alpha.corolla)), "zeta")
  val W = Composite("W", Graft(delta, Vector(Y.target.corolla, Z.target.corolla)), "theta")

  val Psi = Composite("Psi", Graft(W, Vector(Graft(Y, Vector(Leaf(alpha), X.corolla)), Z.corolla, Leaf(delta))), "S")
}

object Example1 {
  val a = Object(1)

  val b = Composite(8, Seed(a), 2)
  val c = Composite(9, b.target.corolla, 3)
  val d = Composite(12, c.target.corolla, 4)
  val e = Composite(14, d.target.corolla, 5)
  val f = Composite(15, e.target.corolla, 6)
  val g = Composite(16, f.target.corolla, 7)

  val h = Composite(24, Leaf(c.target), 10)
  val i = Composite(22, Graft(h.target, Vector(Graft(c, Vector(b.corolla)))), 11)
  val j = Composite(27, d.corolla, 13)
  val k = Composite(29, Graft(g, Vector(f.corolla)), 17)
  val l = Composite(19, Graft(k.target, Vector(Graft(e, Vector(Graft(j.target, Vector(i.target.corolla)))))), 18)

  val m = Composite(36, Leaf(c), 23)
  val n = Composite(35, Graft(i, Vector(Leaf(b), m.target.corolla, h.corolla)), 25)
  val o = Composite(37, Leaf(i.target), 26)
  val p = Composite(38, Leaf(f), 32)
  val q = Composite(40, Leaf(f), 30)
  val r = Composite(39, Graft(k, Vector(q.target.corolla, Leaf(g))), 31)
  val s = Composite(41, Leaf(k.target), 28)
  val t = Composite(43, Leaf(l.target), 20)
  val u = Composite(42, Graft(t.target, Vector(l.corolla)), 21)
  val v = Composite(34, Graft(u.target, Vector(Graft(o.target, Vector(n.target.corolla)), j.corolla, Leaf(e), Graft(s.target, Vector(Graft(r.target, Vector(p.target.corolla, Leaf(g))))))), 33)

  val w = Composite(45, Graft(v, 
    Vector(Graft(n, Vector(m.corolla, Leaf(h), Leaf(i))), o.corolla, Leaf(j), p.corolla, Graft(r, Vector(q.corolla, Leaf(k))), s.corolla, Graft(u, Vector(Leaf(l), t.corolla)))), 44)
}

object Example2 {

  val var0 = Object("0")

  val var3 = Composite("3", Seed(var0), "1")
  val var6 = Composite("6", var3.target.corolla, "2")

  val var10 = Composite("10", var3.corolla, "4")
  val var11 = Composite("11", var6.corolla, "7")
  val var12 = Composite("12", var10.target.corolla, "5")
  val var13 = Composite("13", var11.target.corolla, "8")
  val var14 = Composite("14", Graft(var13.target, Vector(var12.target.corolla)), "9")

  val var17 = Composite("17", Graft(var14, Vector(var12.corolla, var13.corolla)), "15")
  val var18 = Composite("18", Graft(var17.target, Vector(var10.corolla, var11.corolla)), "16")

  val var20 = Composite("20", Graft(var18, Vector(Leaf(var10), Leaf(var11), var17.corolla)), "19")

  val testCardinal = CardinalComplex(var20)
}
