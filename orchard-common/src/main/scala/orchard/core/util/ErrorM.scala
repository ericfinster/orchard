/**
  * ErrorM.scala - Simple Error Monad instances for client side
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.util

import scala.language.implicitConversions

object ErrorM {

  type Error[+A] = Either[String, A] 

  def success[A](a : A) : Error[A] = Right(a)
  def fail[A](msg : String) : Error[A] = Left(msg)

  def sequence[A](errList : List[Error[A]]) : Error[List[A]] = 
    errList match {
      case Nil => success(Nil)
      case e :: es =>
        for {
          f <- e
          fs <- sequence(es)
        } yield (f :: fs)
    }

  def point[A](a : A) : Error[A] = success(a)

  def ensure(prop : Boolean, msg : String) : Error[Unit] = 
    if (prop) success(()) else fail(msg)

  implicit class ErrorOps[A](e : Error[A]) {

    def isSuccess : Boolean = e.isRight
    def isFail : Boolean = e.isLeft

    def get : A = 
      e match {
        case Right(a) => a
        case Left(_) => throw new RuntimeException("Get failed on Error")
      }

    def map[B](f : A => B) : Error[B] = 
      e match {
        case Right(a) => success(f(a))
        case Left(msg) => fail[B](msg)
      }

    def foreach(f : A => Unit) : Unit = 
      e match {
        case Right(a) => f(a)
        case Left(msg) => ()
      }

    def flatMap[B](f : A => Error[B]) : Error[B] =
      e match {
        case Right(a) => f(a)
        case Left(msg) =>  fail[B](msg)
      }

  }

  implicit def optToError[A](opt : Option[A]) : Error[A] = 
    opt match {
      case None => fail("Option was none.")
      case Some(a) => success(a)
    }
}
