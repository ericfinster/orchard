/**
  * EnvironmentSystem.scala - System of environment classes and constructors
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import scala.language.implicitConversions

import orchard.core.ui.Styleable

trait EnvironmentModule { thisChecker : TypeChecker =>

  type EnvironmentEntryType <: EnvironmentEntry
  type IdentifierType <: EnvironmentEntryType with IdentifierEntry
  type GroupType <: EnvironmentEntryType with GroupEntry

  trait EnvironmentEntry extends Styleable {
    thisEntry : EnvironmentEntryType =>

    type LocalEntryType <: ModuleEntryType

    def parentGroup : Option[GroupType]

    def moduleEntry : LocalEntryType

    def name : String = moduleEntry.name
    def qualifiedName : String = moduleEntry.qualifiedName
    def styleString : String = moduleEntry.styleString

  }

  trait IdentifierEntry {
    thisEntry : IdentifierType =>

    type LocalEntryType = ExpressionEntryType

    def liftIdentifier : IdentifierType = this

    def moduleEntry : ExpressionEntry

  }

  trait GroupEntry {
    thisEntry : GroupType =>

    type LocalEntryType = ScopeType

    def liftGroup : GroupType = this

    def entries : Seq[EnvironmentEntryType]

    def moduleEntry : Scope

  }

  object IdentifierEntry {
    implicit def idIsIdentType(idEntry : IdentifierEntry) : IdentifierType = idEntry.liftIdentifier
  }

  object GroupEntry {
    implicit def grpIsGrpType(grpEntry : GroupEntry) : GroupType = grpEntry.liftGroup
  }

  //============================================================================================
  // CONSTRUCTORS
  //

  def newIdentifierEntry(exprEntry : ExpressionEntryType) : IdentifierType
  def newGroupEntry(scope : ScopeType, entries : Seq[EnvironmentEntryType]) : GroupType

}
