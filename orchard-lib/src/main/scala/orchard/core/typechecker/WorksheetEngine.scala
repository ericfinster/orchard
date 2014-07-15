/**
  * WorksheetEngine.scala - A Class which manages a collection of worksheets
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

class WorksheetEngine(val checker : Checker) { thisEngine =>

  private val worksheetMap : Map[String, AbstractWorksheet] = HashMap.empty

  // Okay, the idea is that you call out a worksheet and apply a method to
  // it.  Like dropping and extruding and whatnot.

  // Should the worksheet engine maintain points?  Or selections or whatever?

  // If not, how do we extrude and drop?  That is, how will the outside world
  // tell us the boundary of a desired extrusion?  I think the idea will just
  // be to pass in a list of cell addresses around which you would like to 
  // extrude.  This may be inefficient, but it's the best I can think of, and
  // I'd like to get going on this idea.

  // Hmm. What are the return types going to be here?  How should the outside
  // world refer to the worksheets we create?  It seems we need to return
  // some kind of handle object.  Right.  What we should return is a trait
  // which encapsulates all of the methods you can call on a worksheet, but
  // does not expose the underlying implementation.  For a remote version, you
  // can then implement remote calls with this object which come back to the
  // engine here.

  def newWorksheet(name : String) : Option[Worksheet] = None

}
