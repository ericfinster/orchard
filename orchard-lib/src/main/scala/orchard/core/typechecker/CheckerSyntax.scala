/**
  * CheckerSyntax.scala - Syntax module
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

trait CheckerSyntax { thisChecker : Checker => 

  sealed trait Statement
  case class ModuleStatement(val name : String, val entries : Vector[Statement]) extends Statement
  case class ImportStatment(val name : String, val moduleName : String) extends Statment
  case class ParameterStatement(val identifier : RawIdentifier, val shell : NCell[Option[String]]) extends Statement
  case class DefinitionStatement(val identifier : RawIdentifier, val nook : NCell[Option[String]]) extends Statement

  // Okay, right.  Remember that the idea was to make a free monad out of this guy.
  // Good, yes.  Let's do it this way.  Then you have two things you can do with this.

  // One is that you can implement a zipper which allows you to modify the syntax tree in place.
  // The second is that you can implement and interpreter which progressively builds an environment
  // in which the statements can be interpreted.

}
