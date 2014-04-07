/**
  * Util.scala - Utility objects and classes
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core.util

object Util {
  // Please find a better place to put this ... or a library version?
  implicit class Force[A](opt : Option[A]) {
    def force : A =
      opt match {
        case None => throw new IllegalArgumentException("Force failed.")
        case Some(a) => a
      }

    def force(msg : String) : A =
      opt match {
        case None => throw new IllegalArgumentException(msg)
        case Some(a) => a
      }
  }

  def optSwitch[X](opts : List[Option[X]]) : Option[List[X]] =
    opts match {
      case Nil => Some(Nil)
      case o :: os => for { n <- o ; ns <- optSwitch(os) } yield (n :: ns)
    }

  def optSwitchVect[X](opts : Vector[Option[X]]) : Option[Vector[X]] = {
    try {
      Some(opts map (_.get))
    } catch {
      case e : NoSuchElementException => None
    }
  }

  // We actually use this one all the time.  Perhaps we should make it the default ...
  def invertPerm(p : Vector[Int]) : Vector[Int] = {
    val invP = Range(0, p.length).toVector map (i => (p(i), i))
    invP.sorted map (pr => { val (_, v) = pr ; v })
  }

  var debug : Boolean = false
}
