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

sealed trait NormalizationResult
case object VariableResult extends NormalizationResult
case object FillerResult extends NormalizationResult
case object BoundaryResult extends NormalizationResult

sealed trait Expression extends Stylable {

  def ident : Identifier
  def isThin : Boolean

  def id = {
    if (Util.debug) {
      println("Expanding identifier ...")
    }

    ident.expand
  }

  def name = id

  def ncell : NCell[Expression]

  def normalize : Expression = headNormalize.normalize
  def headNormalize : Expression
  def normalizationResult : NormalizationResult

  var entry : Option[ModuleSystem#ModuleEntry] = None

  def convertsTo(other : Expression) : Boolean =
    this.normalize == other.normalize

}

case class Variable(val shell : Shell, val index : Int, val varIdent : Identifier, val isThin : Boolean) extends Expression {

  def ident = VariableIdentifier(index, varIdent)
  def styleString = if (isThin) "variable-thin" else "variable"

  val ncell : NCell[Expression] =
    shell.withFillingExpression(this)

  override def normalize : Expression = {
    if (Util.debug) println("Normalizing a variable")
    Variable(shell.normalize, index, ident, isThin)
  }

  def headNormalize : Expression = this

  def normalizationResult : NormalizationResult =
    VariableResult

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

  val ident = CompoundIdentifier(List(LiteralIdentifier("def-"), bdryIdent))

  def isThin : Boolean = true
  def styleString = "filler"

  val ncell : NCell[Expression] = 
    nook.withFiller(thisFiller)

  def bdryAddress : CellAddress = 
    nook.framework.topCell.boundaryAddress

  override def normalize : Expression = {
    if (Util.debug) println("Normalizing a filler")
    Filler(nook.normalize, bdryIdent)
  }

  def headNormalize : Expression = thisFiller

  def normalizationResult : NormalizationResult = 
    FillerResult

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

    override def normalize : Expression = {
      if (Util.debug) println("Normalizing a boundary")
      interior.normalize.asInstanceOf[Filler].Boundary
    }

    def headNormalize : Expression = thisBdry

    def normalizationResult : NormalizationResult =
      BoundaryResult

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
    val headNormalExpr = bdryExpr.headNormalize.asInstanceOf[Filler#BoundaryExpr].interior
    val framework = new SimpleFramework(headNormalExpr)

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

  override def normalize = {
    if (Util.debug) println("Normalizing an unfolding")
    referencedExpression.normalize
  }

  def headNormalize =
    referencedExpression.headNormalize

  def normalizationResult =
    referencedExpression.normalizationResult

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

  val topMarker = SubstitutionMarker(expr, Immediate)

  val ncell : NCell[Expression] = {
    val shellFramework = shell.framework.duplicate
    val shellDimension = shellFramework.dimension
    val fillerFramework = shellFramework.newFromExpression(expr)
    shellFramework.stablyAppend(fillerFramework)

    shellFramework.topCell.item = Some(topMarker)

    shellFramework foreachProperFace (cell => {
      cell.item =
        if (cell.dimension < shellDimension) {
          cell.item
        } else {
          Some(SubstitutionMarker(cell.expression.get, cell.address))
        }
    })

    shellFramework.toCell map (_.get)
  }

  def ident : Identifier = topMarker.ident
  def isThin : Boolean = topMarker.isThin
  def styleString : String = topMarker.styleString

  def postCompose(compBindings : Map[Int, Expression]) : Substitution =
    new Substitution(shell, InternalReference(expr), compositeBindings(compBindings))

  def headNormalize : Expression =
    topMarker.headNormalize

  def normalizationResult : NormalizationResult =
    topMarker.normalizationResult

  class SubstitutionIdentifier(body : Identifier) extends ClosureIdentifier(body) {
    def identMap = bindings mapValues (_.ident)
    def wrap(i : Identifier) = new SubstitutionIdentifier(i)
  }

  case class SubstitutionMarker(val localExpression : Expression, val offset : CellAddress) extends Expression { thisMarker =>

    def substitution = thisSubst

    // For now, these use reduction.  We can be smarter about it later ...
    def ident : Identifier = 
      new SubstitutionIdentifier(localExpression.ident)

    def isThin : Boolean = headNormalize.isThin
    def styleString : String = headNormalize.styleString

    // The ncell of this substitution guy simply picks out the subcell
    // of the instantiation ....
    def ncell: NCell[Expression] = 
      thisSubst.ncell.seek(offset).get

    def headNormalize : Expression = 
      localExpression match {
        case v : Variable =>
          if (isBound(v)) {
            getBinding(v).headNormalize
          } else {
            // Ummm... should we translate the identifier as well???
            // Well, yes.  And anyway this is already a bit fishy ....
            v.newVariable((v.shell map ((e : Expression) => new Substitution(shell, InternalReference(e), bindings))),
              v.index, thisMarker.ident, v.isThin)
          }

        case f : Filler =>
          Filler((f.nook map ((e : Expression) => new Substitution(shell, InternalReference(e), bindings))),
            thisMarker.ident)

        case b : Filler#BoundaryExpr =>
          new Substitution(shell, InternalReference(b.interior), bindings).headNormalize.asInstanceOf[Filler].Boundary

        case s : Substitution#SubstitutionMarker => {

          // Here's the explanation: if we see a marker associated to a substitution,
          // we re-instantiate that substitutions with the composite bindings and then seek back
          // to the position of the cell we were looking at.  We can now normalize that cell
          // and we have effectively done a composition!!

          s.substitution.postCompose(bindings).ncell.seek(s.offset).get.value.headNormalize
        }

        case _ => ???
      }

    def normalizationResult : NormalizationResult =
      localExpression match {
        case v : Variable => 
          if (isBound(v)) getBinding(v).normalizationResult else VariableResult
        case _ => localExpression.normalizationResult
      }

    override def toString = "SubstMkr(" ++ offset.toString ++ ", " ++ exprRef.toString ++ 
      ", " ++ localExpression.toString ++ ", " ++ normalizationResult.toString ++ ", " ++ bindings.toString ++ ")"
  }
}

trait SubstitutionOps { thisSubst : Expression => 

  def bindings : Map[Int, Expression]

  def isBound(v : Variable) : Boolean =
    bindings.isDefinedAt(v.index)

  def getBinding(v : Variable) : Expression =
    bindings(v.index)

  def compositeBindings(outerBindings : Map[Int, Expression]) : Map[Int, Expression] = {
    if (Util.debug) {
      println("Composing bindings:")
      println("Inner: " ++ bindings.toString)
      println("Outer: " ++ outerBindings.toString)
    }

    bindings map {
      case (idx, bExpr) => {
        bExpr.normalizationResult match {
          case VariableResult => {
            val headVariable = bExpr.headNormalize.asInstanceOf[Variable]
            if (outerBindings.isDefinedAt(headVariable.index)) {
              if (Util.debug)
                println("Head variable " ++ headVariable.toString ++ " is rebound to " ++ outerBindings(headVariable.index).toString)
              (idx, outerBindings(headVariable.index))
            } else {
              (idx, bExpr)
            }
          }
          case _ => (idx, bExpr)
        }
      }
    }
  }

}

