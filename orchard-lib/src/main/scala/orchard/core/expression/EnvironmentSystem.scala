/**
  * EnvironmentSystem.scala - System of environment classes and constructors
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.ui.Styleable

trait EnvironmentSystem { thisSystem : TypeChecker =>

  type EnvironmentEntryType <: EnvironmentEntry
  type IdentifierType <: EnvironmentEntryType with IdentifierEntry
  type GroupType <: EnvironmentEntryType with GroupEntry

  trait EnvironmentEntry extends Styleable {
    thisEntry : EnvironmentEntryType =>

    def parentGroup : Option[GroupType]

    def qualifiedName : String = 
      parentGroup match {
        case None => name
        case Some(grp) => grp.qualifiedName ++ "/" ++ name
      }
  }

  trait IdentifierEntry {
    thisEntry : IdentifierType =>

  }

  trait GroupEntry {
    thisEntry : GroupType =>

    def entries : Seq[EnvironmentEntryType]

  }

  //============================================================================================
  // CONSTRUCTORS
  //

  def newIdentifierEntry(name : String) : IdentifierType
  def newGroupEntry(name : String, entries : Seq[EnvironmentEntryType]) : GroupType

}
