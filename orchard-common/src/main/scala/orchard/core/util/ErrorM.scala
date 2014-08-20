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

  def succeed[A](a : A) : Error[A] = Right(a)
  def fail[A](msg : String) : Error[A] = Left(msg)

  def branchOn[A, B](e : Error[A])(succeed : A => B, failure : B) : B = 
    e match {
      case Left(_) => failure
      case Right(a) => succeed(a)
    }

  def sequence[A](errList : List[Error[A]]) : Error[List[A]] = 
    errList match {
      case Nil => succeed(Nil)
      case e :: es =>
        for {
          f <- e
          fs <- sequence(es)
        } yield (f :: fs)
    }

  def point[A](a : A) : Error[A] = succeed(a)

  def ensure(prop : Boolean, msg : String) : Error[Unit] = 
    if (prop) succeed(()) else fail(msg)

  def ensureNot(prop : Boolean, msg : String) : Error[Unit] = 
    ensure(! prop, msg)

  def fromOption[A](opt : Option[A], failString : String) : Error[A] =
    opt match {
      case Some(a) => succeed(a)
      case None => fail(failString)
    }

  implicit class ErrorOps[A](e : Error[A]) {

    def isSuccess : Boolean = e.isRight
    def isFailure : Boolean = e.isLeft

    def get : A = 
      e match {
        case Right(a) => a
        case Left(_) => throw new Exception("Get failed on Error")
      }

    def getOrElse(default : A) =
      e match {
        case Right(a) => a
        case Left(_) => default
      }

    def map[B](f : A => B) : Error[B] = 
      e match {
        case Right(a) => succeed(f(a))
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

    def filter(p : A => Boolean) : Error[A] = 
      e match {
        case Right(a) => if (p(a)) succeed(a) else fail("Filter failed")
        case Left(msg) => fail(msg)
      }

  }

  implicit def optToError[A](opt : Option[A]) : Error[A] = 
    opt match {
      case None => fail("Option was none.")
      case Some(a) => succeed(a)
    }

  implicit def errorIsReadable[A, P](implicit aReader : JsonReadable[A, P]) : JsonReadable[Error[A], P] =
    new JsonReadable[Error[A], P] {
      def read(x : P, reader : JsonReader[P]) : Error[A] = {
        reader.readString(reader.readObjectField(x, "type")) match {
          case "succeed" => Right(aReader.read(reader.readObjectField(x, "value"), reader))
          case "failure" => Left(reader.readString(reader.readObjectField(x, "value")))
        }
      }
    }

  implicit def errorIsWritable[A, P](implicit aWriter : JsonWritable[A, P]) : JsonWritable[Error[A], P] =
    new JsonWritable[Error[A], P] {
      def write(e : Error[A], writer : JsonWriter[P]) : P = {
        e match {
          case Right(a) => {
            writer.writeObject(
              "type" -> writer.writeString("succeed"),
              "value" -> aWriter.write(a, writer)
            )
          }
          case Left(msg) => {
            writer.writeObject(
              "type" -> writer.writeString("failure"),
              "value" -> writer.writeString(msg)
            )
          }
        }
      }
    }
}
