/**
  * Events.scala - Events and whatnot
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core

import scala.collection.mutable.WeakHashMap

trait EventEmitter[A] {

  val reactors : WeakHashMap[EventReactor[A], Unit] =
    new WeakHashMap[EventReactor[A], Unit]

  def emit(ev : A) = {
    reactors.keys foreach (reactor => reactor.onEventEmitted(ev))
  }
}

trait EventReactor[A] {

  def reactTo(emitter : EventEmitter[A]) = {
    emitter.reactors(this) = ()
  }

  def onEventEmitted(ev : A) : Unit 
}

trait EventConduit[A] extends EventReactor[A] with EventEmitter[A] {
  // By default, pass the event to any listeners ...
  def onEventEmitted(ev : A) = emit(ev)
}


//  Some thought on our current event setup:
//
//  The problem right now is that the event hierarchy and the inheritance hierarchy are linked in
//  a kind of silly way.  You should really have something more like JavaFX, where, instead of overriding
//  a handling method, you pass listeners in which are called automatically.  That way, super classes 
//  can register the event handling that they must, and we will only see the kind we want bubbled down ...
