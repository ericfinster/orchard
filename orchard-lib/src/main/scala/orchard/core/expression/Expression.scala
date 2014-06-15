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

  def substituteAndReduce(bindings : Map[Int, Expression]) : Expression = 
    if (bindings.isDefinedAt(index)) {
      bindings(index).reduce
    } else {
      Variable(shell map (Substitution(_, bindings).reduce), index, ident, isThin)
    }

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

  def bdryAddress : CellAddress = 
    nook.framework.topCell.boundaryAddress

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
    def styleString = if (thisBdry.isThin) "bdry-thin" else "bdry"

    def ncell : NCell[Expression] =
      thisFiller.ncell.seek(bdryAddress).get

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

case class Unfolding(val subst : Substitution, val addr : CellAddress) extends Expression {

  assert(subst.normalizationStore.resultType == BoundaryType)

  def addressDictionary : NCell[(Expression, CellAddress)] = {
    val filler = subst.normalize.asInstanceOf[Filler#BoundaryExpr].interior
    val framework = new SimpleFramework(filler)
    framework.topCell.skeleton map (cell => (cell.expression.get, cell.address))
  }

  def referencedExpression : Expression = 
    addressDictionary.seek(addr).get.value._1

  def ident = referencedExpression.ident
  def isThin = referencedExpression.isThin
  def styleString = referencedExpression.styleString

  def ncell : NCell[Expression] =
    addressDictionary.seek(addr).get map {
      case (_, localAddr) => Unfolding(subst, localAddr)
    }

  def reduce = 
    addressDictionary.seek(addr).get.value._1

  def unfold = 
    addressDictionary.seek(addr).get.value._1

  override def toString = "Unfolding(" ++ subst.id ++ ", " ++ addr.toString ++ ")"
}

case class Reference(val lift : ModuleSystem#Lift, val addr : CellAddress) extends Expression {

  def addressDictionary : NCell[(Expression, CellAddress)] = {
    val framework = new SimpleFramework(lift.filler)
    framework.topCell.skeleton map (cell => (cell.expression.get, cell.address))
  }

  def referencedExpression : Expression = 
    addressDictionary.seek(addr).get.value._1

  def ident: Identifier = referencedExpression.ident
  def isThin: Boolean = referencedExpression.isThin

  // Perhaps some efficiency danger lurks here ...
  def ncell: NCell[Expression] =
    addressDictionary.seek(addr).get map {
      case (_, localAddr) => Reference(lift, localAddr)
    }

  def unfold : Expression = 
    referencedExpression.unfold

  // Reduction is blocked by folded references
  def reduce : Expression = this

  def styleString: String = referencedExpression.styleString

  override def toString = "Ref(" ++ lift.name ++ ", " ++ addr.toString ++ ")"
}

// The point also is that every face of a substitution builds it's own normalization.  But this
// is horribly redundant: once the main substitution has normalized, we should simply save the
// results (or use a "flag" shell) and have the faces look them up.

// Also, substitutions should have a shell parameter which we have not added yet.  So it's time to
// revisit this guy and give a more efficient implementation ....

case class Substitution(val expr : Expression, val bindings : Map[Int, Expression]) extends Expression {

  var myStore : Option[NormalizationStore] = None

  def normalizationStore : NormalizationStore =
    myStore match {
      case None => {
        val store = buildNormalizationStore
        myStore = Some(store)
        store
      }
      case Some(store) => store
    }

  def buildNormalizationStore : NormalizationStore = {
    val normalizedExpression = normalize

    normalizedExpression match {
      case v : Variable => NormalizationStore(VariableType, v.isThin, v.id, v.styleString)
      case f : Filler => NormalizationStore(FillerType, f.isThin, f.id, f.styleString)
      case b : Filler#BoundaryExpr => NormalizationStore(BoundaryType, b.isThin, b.id, b.styleString)
      case _ => throw new IllegalStateException("Normalization did not finish somehow ...")
    }
  }

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

  // Mmmm..... if one of the output bindings is in fact a substitution, it won't get unfolded.  I think
  // this is the problem you're seeing ...

  // The other point is that as long as you are separating normalization and unfolding, this unfolding
  // happens twice while in reality it should only happen on one of the two branches.
  def unfold : Expression = 
    Substitution(expr.unfold, bindings mapValues (_.unfold))


  // This is where the substitution work goes ...
  def reduce : Expression =
    expr match {
      case v : Variable => 
        v.substituteAndReduce(bindings)

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
              if (isBound(v))
                (idx, getBinding(v))
              else
                (idx, v)
            }
            case (idx, bExpr) => (idx, bExpr)
          }

        Substitution(s.expr, composedBindings).reduce
      }

      // Don't substitute in unfoldings either
      case u : Unfolding => this
    }

  // Here we could definitely use some optimization.  But for 
  // now let's do the obvious thing so that we can check out
  // whether our reduction algorithm is giving the right answers
  def isThin: Boolean = normalizationStore.isThin
  def styleString : String = normalizationStore.styleString

  override def toString = "Subst(" ++ expr.toString ++ "," ++ bindings.toString ++ ")"

}

//============================================================================================
// NORAMLIZATION FLAGS
//

sealed trait NormalizationType
case object VariableType extends NormalizationType
case object FillerType extends NormalizationType
case object BoundaryType extends NormalizationType

case class NormalizationStore(val resultType : NormalizationType, val isThin : Boolean, val identString : String, val styleString : String)

