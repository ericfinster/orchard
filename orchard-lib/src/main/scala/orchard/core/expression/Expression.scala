/**
  * Expression.scala - Expression definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._
import orchard.core.ui.Stylable

sealed trait Expression extends Stylable {

  def ident : Identifier
  def isThin : Boolean

  def id = ident.toString
  def name = id

  def ncell : NCell[Expression]

  def unfold : Expression
  def reduce : Expression

  def normalize : Expression = this.unfold.reduce

  def convertsTo(other : Expression) : Boolean =
    this.normalize == other.normalize

}

case class Variable(val shell : Shell, val index : Int, val ident : Identifier, val isThin : Boolean) extends Expression {

  def styleString = if (isThin) "var-thin" else "var"

  val ncell : NCell[Expression] =
    shell.withFillingExpression(this)

  def unfold : Expression = Variable(shell map (_.unfold), index, ident, isThin)
  def reduce : Expression = Variable(shell map (_.reduce), index, ident, isThin)

  //============================================================================================
  // DEFINITIONAL EQUALITY
  //

  def canEqual(other : Any) : Boolean = 
    other.isInstanceOf[Variable]

  override def equals(other : Any) : Boolean = 
    other match {
      case that : Variable =>
        (that canEqual this) && (that.shell == this.shell) && (that.index == this.index)
      case _ => false
    }

  override def hashCode : Int = 
    41 * (
      41 * (
        41 + shell.hashCode
      ) + index.hashCode
    )

  override def toString = "Var(" ++ id ++ ")"
}

case class Filler(val nook : Nook, bdryIdent : Identifier) extends Expression { thisFiller =>

  val ident = ExpressionIdentifier(LiteralToken("def-") :: bdryIdent.tokens)
  def isThin : Boolean = true
  def styleString = "filler"

  val ncell : NCell[Expression] = 
    nook.withFiller(thisFiller)

  def unfold : Expression = Filler(nook map (_.unfold), bdryIdent)
  def reduce : Expression = Filler(nook map (_.reduce), bdryIdent)

  //============================================================================================
  // DEFINITIONAL EQUALITY
  //

  def canEqual(other : Any) : Boolean = 
    other.isInstanceOf[Filler]

  override def equals(other : Any) : Boolean = 
    other match {
      case that : Filler =>
        (that canEqual this) && (that.nook == this.nook)
      case _ => false
    }

  override def hashCode : Int = 
    41 * ( 41 + nook.hashCode )

  override def toString = "Fill(" ++ id ++ ")"

  //============================================================================================
  // BOUNDARY DEFINITION
  //

  trait BoundaryExpr extends Expression { thisBdry =>

    val ident = bdryIdent
    val interior = thisFiller
    def isThin = nook.isThinBoundary
    def styleString = if (isThin) "bdry-thin" else "bdry"

    val ncell : NCell[Expression] =
      nook.withBoundary(thisBdry)

    def unfold : Expression = 
      interior.unfold.asInstanceOf[Filler].Boundary

    def reduce : Expression = 
      interior.reduce.asInstanceOf[Filler].Boundary

    def canEqual(other : Any) : Boolean =
      other.isInstanceOf[Filler#BoundaryExpr]

  }

  case object Boundary extends BoundaryExpr {

    //============================================================================================
    // DEFINITIONAL EQUALITY
    //

    override def equals(other : Any) : Boolean =
      other match {
        case that : Filler#BoundaryExpr =>
          (that canEqual this) && (that.interior == this.interior)
        case _ => false
      }

    override def hashCode : Int =
      41 * ( 41 + interior.hashCode )

    override def toString = "Bdry(" ++ id ++ ")"

  }

}

case class Reference(val defn : ModuleSystem#Definition, val addr : CellAddress) extends Expression {

  assert(defn.isComplete)

  def addressDictionary : NCell[(Expression, CellAddress)] = {
    val framework = new SimpleFramework(defn.filler.get)
    framework.topCell.skeleton map (cell => (cell.expression.get, cell.address))
  }

  def referencedExpression : Expression = 
    addressDictionary.seek(addr).get.value._1

  def ident: Identifier = referencedExpression.ident
  def isThin: Boolean = referencedExpression.isThin

  // Perhaps some efficiency danger lurks here ...
  def ncell: NCell[Expression] =
    addressDictionary.seek(addr).get map {
      case (_, localAddr) => Reference(defn, localAddr)
    }

  def unfold : Expression = 
    referencedExpression.unfold

  // Reduction is blocked by folded references
  def reduce : Expression = this

  def styleString: String = referencedExpression.styleString

  override def toString = "Ref(" ++ defn.name ++ ", " ++ addr.toString ++ ")"
}

case class Substitution(val expr : Expression, val bindings : Map[Int, Expression]) extends Expression {

  def translateTokens(toks : List[IdentToken]) : List[IdentToken] =
    toks map {
      case et @ ExpressionToken(Variable(_, idx, _, _)) =>
        if (bindings.isDefinedAt(idx)) {
          ExpressionToken(bindings(idx))
        } else et
      case tok @ _ => tok
    }

  // Fix this to translate variable identifiers correctly ...
  def translateIdent(id : Identifier) : Identifier = {
    id match {
      case vIdent : VariableIdentifier =>
        if (bindings.isDefinedAt(vIdent.index)) {
          // We do not translate the resulting identifier here.  I think it's
          // entries should be out of the scope of this substitution ....
          bindings(vIdent.index).ident
        } else {
          VariableIdentifier(vIdent.index, translateTokens(vIdent.tokens))
        }
      case eIdent : ExpressionIdentifier => 
        ExpressionIdentifier(translateTokens(eIdent.tokens))
    }
  }

  def ident : Identifier = translateIdent(expr.ident)

  def ncell: NCell[Expression] = 
    expr.ncell map (Substitution(_, bindings))

  def unfold : Expression = 
    Substitution(expr.unfold, bindings)

  // This is where the substitution work goes ...
  def reduce : Expression =
    expr match {
      case v : Variable => {
        if (bindings.isDefinedAt(v.index)) {
          bindings(v.index).reduce
        } else {
          // Do we need to pass the substitution on to the shell first????
          Variable(v.shell map (Substitution(_, bindings).reduce), v.index, ident, v.isThin)
        }
      }

      case f : Filler =>
        Filler(f.nook map (Substitution(_, bindings).reduce), translateIdent(f.bdryIdent))

      case b : Filler#BoundaryExpr => 
        Substitution(b.interior, bindings).reduce.asInstanceOf[Filler].Boundary

      // On a reference, we can't reduce
      case r : Reference => this

      case s : Substitution => {
        // Compose the two subtitutions by creating a new map
        // How does the map composition work????

        // The inner substitution may bind some variables to variables which might be
        // rebound.  We compose where it is possible.  Do we need to keep the rest of
        // the outer substitution?

        val composedBindings : Map[Int, Expression] =
          s.bindings map {
            case (idx, v : Variable) => {
              if (bindings.isDefinedAt(v.index)) {
                (idx, bindings(v.index))
              } else {
                (idx, v)
              }
            }
            case (idx, bExpr) => (idx, bExpr)
          }

        Substitution(s.expr, composedBindings)
      }

    }

  def isThin: Boolean = 
    expr match {
      case v : Variable =>
        if (bindings.isDefinedAt(v.index)) {
          bindings(v.index).isThin
        } else {
          v.isThin
        }
      case b : Filler#BoundaryExpr => normalize.isThin
      case _ => expr.isThin
    }

  def styleString: String =
    expr match {
      case v : Variable =>
        if (bindings.isDefinedAt(v.index)) {
          bindings(v.index).styleString
        } else {
          v.styleString
        }
      case b : Filler#BoundaryExpr => normalize.styleString
      case _ => expr.styleString
    }

  override def toString = "Subst(" ++ expr.toString ++ "," ++ bindings.toString ++ ")"

}
