/**
  * Syntax.scala - Definitions of Syntactic Elements
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._

sealed trait Statement
case class ModuleDefinition(name : String, body : List[Statement]) extends Statement
case class VariableDefinition(name : String, shell : Shell) extends Statement
case class LiftDefinition(name : String, nook : Nook) extends Statement
case class Import(moduleName : String) extends Statement

// sealed trait Expression

// The idea is that nooks and shells will now have identifiers in their shells.  Or
// perhaps rather "expressions" which are entities which evaluate to the kinds of
// expressions you had before.

// Now the question arises about the rewriting of names.  So like, how should this work?
// I think it's like before, where the identifiers contain environment references.

// Or is there some kind of difference between giving a unique identifier to a lift or
// an expression, and rather having it depend on some kind of parameters.  This is the
// problem of how you want to treat abstraction.
