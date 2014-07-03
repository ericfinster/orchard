/**
  * EnvironmentSystem.scala - System of environment classes and constructors
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.ui.Styleable

trait EnvironmentModule { thisChecker : TypeChecker =>

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

    def moduleEntry : ModuleEntry

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

  def newIdentifierEntry(name : String, moduleEntry : ModuleEntryType) : IdentifierType
  def newGroupEntry(name : String, moduleEntry : ModuleEntryType, entries : Seq[EnvironmentEntryType]) : GroupType

}
