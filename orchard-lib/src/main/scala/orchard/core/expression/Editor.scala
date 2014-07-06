/**
  * Editor.scala - Editor trait 
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

trait Editor {

  def withAssumptionInfo[A](
    thinHint : Boolean,
    forceThin : Boolean,
    handler : (String, Boolean) => CheckerResult[A]
  ) : Unit

  def withFillerIdentifier[A](
    handler : String => CheckerResult[A]
  ) : Unit

  // def withFillerIdentifiers(handler : (String, String) => Unit) : Unit
  // def withRenameIdentifier(expr : Expression, handler : String => Unit) : Unit

  def consoleWrite(str : String) : Unit 
  def consoleMessage(str : String) : Unit
  def consoleError(str : String) : Unit
  def consoleDebug(str : String) : Unit

  def executeCheckerCommand[A](cmd : CheckerResult[A]) : Unit = 
    cmd match {
      case CheckerSuccess(_) => ()
      case CheckerFailure(e) => {
        if (e.getMessage == null) {
          consoleError(e.getStackTraceString)
        } else
          consoleError(e.getMessage)
      }
    }

  // I don't quite see how to set up the IO mechanism yet.  Let's move on with the
  // ad-hoc system you have now and come back to this ...

  // sealed trait IOResult[+A] {

  //   def map[B](f : A => B) : IOResult[B]
  //   def flatMap[B](f : A => IOResult[B]) : IOResult[B]
  //   def filter(f : A => Boolean) : IOResult[A]
  //   def foreach(f : A => Unit) : Unit

  // }

  // case class IOSuccess[+A](result : A) extends IOResult[A] {

  //   def map[B](f : A => B) : IOResult[B] =
  //     IOSuccess(f(result))

  //   def flatMap[B](f : A => IOResult[B]) : IOResult[B] =
  //     f(result)

  //   def filter(f : A => Boolean) : IOResult[A] =
  //     if (f(result)) this else IOFailure("Result was filtered")

  //   def foreach(f : A => Unit) : Unit =
  //     f(result)

  // }

  // case class IOFailure(cause : String) extends IOResult[Nothing] {

  //   def map[B](f : Nothing => B) : IOResult[B] = this
  //   def flatMap[B](f : Nothing => IOResult[B]) : IOResult[B] = this
  //   def filter(f : Nothing => Boolean) : IOResult[Nothing] = this
  //   def foreach(f : Nothing => Unit) : Unit = ()

  // }

}
