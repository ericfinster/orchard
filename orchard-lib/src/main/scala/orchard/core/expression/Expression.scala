/**
  * Expression.scala - Expression definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._
import orchard.core.util._
import orchard.core.ui.Stylable

sealed trait Expression extends Stylable {

  def ident : Identifier
  def isThin : Boolean

  def id = ident.toString
  def name = id

  def ncell : NCell[Expression]

  def normalize : Expression

  def convertsTo(other : Expression) : Boolean =
    this.normalize == other.normalize

}

case class Variable(val shell : Shell, val index : Int, val ident : Identifier, val isThin : Boolean) extends Expression {

  def styleString = if (isThin) "var-thin" else "var"

  val ncell : NCell[Expression] =
    shell.withFillingExpression(this)

  def normalize : Expression = {
    if (Util.debug) println("Normalizing a variable")
    Variable(shell.normalize, index, ident, isThin)
  }

  def newVariable(s : Shell, i : Int, idnt : Identifier, isThn : Boolean) = 
    Variable(s, i, idnt, isThn)

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

  override def toString = "Var(" ++ id ++ ", " ++ index.toString ++ ", " ++ isThin.toString ++ ")"
}

case class Filler(val nook : Nook, bdryIdent : Identifier) extends Expression { thisFiller =>

  val ident = ExpressionIdentifier(LiteralToken("def-") :: bdryIdent.tokens)
  def isThin : Boolean = true
  def styleString = "filler"

  val ncell : NCell[Expression] = 
    nook.withFiller(thisFiller)

  def bdryAddress : CellAddress = 
    nook.framework.topCell.boundaryAddress

  def normalize : Expression = {
    if (Util.debug) println("Normalizing a filler")
    Filler(nook.normalize, bdryIdent)
  }

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
    def styleString = if (thisBdry.isThin) "bdry-thin" else "bdry"

    def ncell : NCell[Expression] =
      thisFiller.ncell.seek(bdryAddress).get

    def normalize : Expression = {
      if (Util.debug) println("Normalizing a boundary")
      interior.normalize.asInstanceOf[Filler].Boundary
    }

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

case class Unfolding(bdryExpr : Expression, addr : CellAddress) extends Expression {

  val addressDictionary : NCell[(Expression, CellAddress)] = {
    val normalizedExpression = bdryExpr.normalize.asInstanceOf[Filler#BoundaryExpr].interior
    val framework = new SimpleFramework(normalizedExpression)

    framework.topCell.skeleton map (cell => {
      (cell.expression.get, cell.address)
    })
  }

  def referencedExpression : Expression = 
    addressDictionary.seek(addr).get.value._1

  def ident : Identifier = referencedExpression.ident
  def isThin : Boolean = referencedExpression.isThin
  def styleString : String = referencedExpression.styleString

  def ncell : NCell[Expression] =
    addressDictionary.seek(addr).get map {case (_, localAddr) => Unfolding(bdryExpr, localAddr) }

  def normalize = {
    if (Util.debug) println("Normalizing an unfolding")
    referencedExpression
  }

  override def toString = "Unfolding(" ++ bdryExpr.toString ++ ", " ++ addr.toString ++ ")"

}

sealed trait ExpressionReference { def expression : Expression }

case class ExternalReference(entry : ModuleSystem#ExpressionEntry) extends ExpressionReference {
  def expression = entry.asInstanceOf[ModuleSystem#Lift].filler

  override def toString = "ExtRef(" ++ expression.id ++ ")"
}

case class InternalReference(expr : Expression) extends ExpressionReference {
  def expression = expr

  override def toString = "IntRef(" ++ expression.id ++ ")"
}

case class Substitution(val shell : Shell, val exprRef : ExpressionReference, val bindings : Map[Int, Expression]) 
    extends Expression with SubstitutionOps { thisSubst =>

  def expr : Expression = exprRef.expression

  def ident : Identifier = translateIdent(expr.ident)
  def isThin : Boolean = normalizedExpression.isThin
  def styleString : String = normalizedExpression.styleString

  def ncell : NCell[Expression] = {
    val shellFramework = shell.framework.duplicate
    val shellDimension = shellFramework.dimension
    val fillerFramework = shellFramework.newFromExpression(expr)
    shellFramework.stablyAppend(fillerFramework)

    shellFramework forAllCells (cell => {
      cell.item =
        if (cell.dimension < shellDimension) {
          cell.item
        } else {
          Some(SubstitutionMarker(cell.address))
        }
    })

    shellFramework.toCell map (_.get)
  }

  def postCompose(compBindings : Map[Int, Expression]) : Substitution =
    new Substitution(shell, InternalReference(expr), compositeBindings(compBindings))

  def normalize : Expression = 
    expr match {
      case v : Variable => 
        if (isBound(v)) {
          getBinding(v).normalize
        } else {
          // Ummm... should we translate the identifier as well???
          // Well, yes.  And anyway this is already a bit fishy ....
          v.newVariable((v.shell map ((e : Expression) => new Substitution(shell, InternalReference(e), bindings))).normalize,
            v.index, v.ident, v.isThin)
        }

      case f : Filler =>
        Filler((f.nook map ((e : Expression) => new Substitution(shell, InternalReference(e), bindings))).normalize, 
          translateIdent(f.bdryIdent))

      case b : Filler#BoundaryExpr => 
        new Substitution(shell, InternalReference(b.interior), bindings).normalize.asInstanceOf[Filler].Boundary

      case s : Substitution#SubstitutionMarker => {

        // Here's the explanation: if we see a marker associated to a substitution,
        // we re-instantiate that substitutions with the composite bindings and then seek back
        // to the position of the cell we were looking at.  We can now normalize that cell
        // and we have effectively done a composition!!

        s.substitution.postCompose(bindings).ncell.seek(s.offset).get.value.normalize
      }

      case _ => ???
    }

  case class SubstitutionMarker(val offset : CellAddress) extends Expression {

    def substitution = thisSubst

    def referencedExpression : Expression = 
      normalizedExpression.ncell.seek(offset).get.value

    // For now, these use reduction.  We can be smarter about it later ...
    def ident : Identifier = referencedExpression.ident
    def isThin : Boolean = referencedExpression.isThin
    def styleString : String = referencedExpression.styleString

    // The ncell of this substitution guy simply picks out the subcell
    // of the instantiation ....
    def ncell: NCell[Expression] = 
      thisSubst.ncell.seek(offset).get

    def substExpr : Expression = 
      expr.ncell.seek(offset).get.value

    def normalize : Expression = {
      if (Util.debug) println("Normalizing a substitution")

      offset match {
        case Immediate => thisSubst.normalize
        case _ => new Substitution(shell, InternalReference(substExpr), bindings).normalize
      }
    }

    override def toString = "SubstMkr(" ++ offset.toString ++ ", " ++ exprRef.toString ++ ")"
  }
}

trait SubstitutionOps { thisSubst : Expression => 

  var myNormalizedExpression : Option[Expression] = None

  def normalizedExpression : Expression = 
    myNormalizedExpression match {
      case None => {
        val ne = normalize
        myNormalizedExpression = Some(ne)
        ne
      }
      case Some(ne) => ne
    }

  def bindings : Map[Int, Expression]

  def isBound(v : Variable) : Boolean =
    bindings.isDefinedAt(v.index)

  def getBinding(v : Variable) : Expression =
    bindings(v.index)

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
    val resultIdent = 
      id match {
        case vIdent : VariableIdentifier =>
          if (bindings.isDefinedAt(vIdent.index)) {
            bindings(vIdent.index).ident
          } else {
            if (Util.debug) println("Translating an unbound variable!")
            VariableIdentifier(vIdent.index, translateTokens(vIdent.tokens))
          }
        case eIdent : ExpressionIdentifier =>
          ExpressionIdentifier(translateTokens(eIdent.tokens))
      }

    if (Util.debug)
      println("Tralating identifier " ++ id.toString ++ " to " ++ resultIdent.toString)

    resultIdent
  }

  def compositeBindings(outerBindings : Map[Int, Expression]) : Map[Int, Expression] = 
    bindings map {
      case (idx, v : Variable) => {
        if (outerBindings.isDefinedAt(v.index))
          (idx, outerBindings(v.index))
        else
          (idx, v)
      }
      case (idx, bExpr) => (idx, bExpr)
    }

}
