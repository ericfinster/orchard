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
case class Parameter(ident : Identifier, varExpr : Variable) extends Statement
case class Definition(ident : Identifier, liftExpr : Lift) extends Statement
case class Import(module : String) extends Statement

sealed trait Expression
case class Reference(ref : String) extends Expression
case class Substitution(shell : Shell, expr : Expression, bindings : Map[Int, Expression]) extends Expression
case class DefinitionOf(expr : Expression) extends Expression
case class Face(expr : Expression, faceAddress : CellAddress) extends Expression
case class Lift(nook : Nook) extends Expression
case class Variable(index : Int, shell : Shell) extends Expression
case class Let(ident : Identifier, value : Expression, expr : Expression)

sealed trait Identifier

class Nook(ncell : NCell[Option[Expression]])
class Shell(ncell : NCell[Option[Expression]])

// The point of the above is that it is essentially a languge for describing opetopic definitions.  Neat!
// Now what you have to do is to write a compiler/type-checker which checks whether a give collection of
// statements makes sense.  And then write the user-interface (which should remain completely separate)
// which allows users to build well-formed expressions with ease (since this syntax is for the most part
// completely unusable!!)

// And there are some *great* ideas to go with this!  Like consider the following: it should be trivial
// to write this to xml.  But then you can do the following neat trick: use stylesheets or translations
// or whatever to *render the code as html*, meaning with svg's and everything so that instead of the
// horrible syntax which is going to define shells and ncells and whatnot, you simply have an svg image
// generated.  So the raw code editor becomes your browser!  This will take care of rendering the pretty
// pictures and everything.  Oh that's fucking brilliant!

// Of course this is just more evidence that you need to write a javascript version of the rendering algorithm
// so that the whole thing can be done in the browser.

// Okay, but this will just have to be later.  For now you're just going to have to use the javafx ui to
// test out if this is even plausible or not.  Then you can add the fancy features like that ....


