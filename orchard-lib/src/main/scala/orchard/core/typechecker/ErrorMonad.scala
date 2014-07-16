/**
  * ErrorMonad.scala - Some helper functions for Errors
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scalaz._

trait ErrorMonad {

  type Error[+A] = \/[String, A]

  def succeed[A](a : A) : Error[A] = \/-(a)
  def fail[A](str : String) : Error[A] = -\/(str)

  def extractOption[A](opt : Option[A], failString : String) : Error[A] = 
    opt match {
      case None => fail(failString)
      case Some(a) => succeed(a)
    }

}
