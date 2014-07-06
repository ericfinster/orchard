/**
  * CheckerResult.scala - Result monad for type checker
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import scala.util.control.NonFatal
import scala.language.implicitConversions

sealed abstract class CheckerResult[+A] { self =>

  def isSuccess : Boolean
  def isFailure : Boolean

  def get : A

  def getOrElse[B >: A](default : => B) : B = 
    if (isSuccess) get else default

  def orElse[B >: A](default : => CheckerResult[B]) : CheckerResult[B] = 
    try if (isSuccess) this else default
    catch {
      case NonFatal(e) => CheckerFailure(e)
    }

  def foreach[U](f : A => U) : Unit 

  def flatMap[B](f : A => CheckerResult[B]) : CheckerResult[B]

  def map[B](f : A => B) : CheckerResult[B]

  def filter(p : A => Boolean) : CheckerResult[A]

  def toOption : Option[A] = if (isSuccess) Some(get) else None

  def flatten[B](implicit ev : A <:< CheckerResult[B]): CheckerResult[B]

  // def withFilter(p: A => Boolean) : WithFilter = new WithFilter(p)

  // def iterator : Iterator[A]

  // // Not sure exactly why we need this, but seems to help in intertion
  // class WithFilter(p: A => Boolean) {
  //   def map[B](f: A => B): CheckerResult[B] = self filter p map f
  //   def flatMap[B](f: A => CheckerResult[B]): CheckerResult[B] = self filter p flatMap f
  //   def foreach[U](f: A => U): Unit = self filter p foreach f
  //   def withFilter(q: A => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
  // }  

  // def toList : List[A]

}

case class CheckerSuccess[+A](result : A) extends CheckerResult[A] {

  def isSuccess : Boolean = true
  def isFailure : Boolean = false

  def get : A = result

  def map[B](f : A => B) : CheckerResult[B] =
    CheckerResult[B](f(result))

  def flatMap[B](f : A => CheckerResult[B]) : CheckerResult[B] =
    try f(result)
    catch {
      case NonFatal(e) => CheckerFailure(e)
    }

  def filter(f : A => Boolean) : CheckerResult[A] = 
    try {
      if (f(result)) this else 
        CheckerFailure("Predicate failed for value " + result)
    } catch {
      case NonFatal(e) => CheckerFailure(e)
    }

  def foreach[U](f : A => U) : Unit = f(result)

  def flatten[B](implicit ev : A <:< CheckerResult[B]): CheckerResult[B] =
    result

  def iterator : Iterator[A] = 
    collection.Iterator.single(result)

  def toList : List[A] = List(result)

}

//import java.lang.RuntimeException

object CheckerFailure {

  def apply(cause : String) = new CheckerFailure(cause)

}

case class CheckerFailure[+A](val exception : Throwable) extends CheckerResult[A] {

  def this(cause : String) = this(new RuntimeException(cause))

  def isSuccess : Boolean = false
  def isFailure : Boolean = true

  def get : A = throw exception

  def map[B](f : A => B) : CheckerResult[B] = this.asInstanceOf[CheckerResult[B]]
  def flatMap[B](f : A => CheckerResult[B]) : CheckerResult[B] = this.asInstanceOf[CheckerResult[B]]
  def filter(f : A => Boolean) : CheckerResult[A] = this
  def foreach[U](f : A => U) : Unit = ()

  def flatten[B](implicit ev : A <:< CheckerResult[B]): CheckerResult[B] =
    this.asInstanceOf[CheckerResult[B]]

  def iterator : Iterator[Nothing] =
    collection.Iterator.empty

  def toList : List[Nothing] = Nil

}

object CheckerResult {

  implicit def checker2Iterable[A](cr: CheckerResult[A]): Iterable[A] = cr.toList

  def apply[A](r : => A) : CheckerResult[A] = 
    try CheckerSuccess(r) catch {
      case NonFatal(e) => CheckerFailure(e)
    }

}
