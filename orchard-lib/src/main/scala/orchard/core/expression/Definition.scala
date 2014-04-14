/**
  * Definition.scala - Definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import orchard.core.cell.NCell
import orchard.core.util.XmlSerializable

class Definition(
  val envRoot : SimpleEnvironmentNode,
  val stabilityLevel : Option[Int],
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int]
) extends HasEnvironment {

  type EnvironmentNodeType = SimpleEnvironmentNode

  def envOps = SimpleNodeImplementation

  def name : String = envRoot.element.asInstanceOf[GroupElement].name


}

object Definition {

  def identifierToXML(ident : Identifier) : xml.Node = 
    <identifier>{ident.tokens map {
      case LiteralToken(lit) => <lit>{xml.Text(lit)}</lit>
      case ExpressionToken(expr) => <ref id={expr.hashCode.toString} />
    }}</identifier>

  def rawIdentifierFromXML(node : xml.Node) : RawIdentifier =
    node match {
      case <identifier>{identContent @ _*}</identifier> => {
        val tokens =
          identContent map {
            case <lit>{litContent}</lit> => RawLiteral(litContent.text)
            case r @ <ref/> => RawReference((r \ "@id").text)
          }

        RawIdentifier(tokens.toList)
      }
    }

  def expressionToXML(expr : Expression) : xml.NodeSeq = 
    expr match {
      case Variable(ident, isThin) => 
        <variable isThin={isThin.toString} id={expr.hashCode.toString}>{
          identifierToXML(ident)
        }</variable>
      case f @ Filler(ident, bdryIdent, bdryIsThin) =>
        <filler bdryIsThin={bdryIsThin.toString} 
                bdryId={f.MyBoundary.hashCode.toString}
                id={expr.hashCode.toString}>{
          <fillerIdent>{identifierToXML(ident)}</fillerIdent>
          <bdryIdent>{identifierToXML(bdryIdent)}</bdryIdent>
        }</filler>
      case bdry : Filler#Boundary => xml.NodeSeq.Empty
    }

  def definitionToXML(definition : Definition) : xml.Node = {
    def optToInt(opt : Option[Int]) : Int =
      opt getOrElse -1

    <definition
       name={definition.name}
       slevel={optToInt(definition.stabilityLevel).toString}
       ilevel={optToInt(definition.invertibilityLevel).toString}
       ulevel={optToInt(definition.unicityLevel).toString}
    ><expressions>{
      definition.envOps.toExprSeq(definition.envRoot) map (expressionToXML(_))
    }</expressions><environment>{
      definition.envOps.toXML(definition.envRoot)
    }</environment></definition>
  }

  def fromXML(node : xml.Node) : Definition = {
    val exprMap : Map[Int, Expression] = new HashMap
    val identMap : Map[Int, RawIdentifier] = new HashMap

    def readExpression(node : xml.Node) : Unit =
      node match {
        case v @ <variable>{identContent}</variable> => {

          val ident = rawIdentifierFromXML(identContent)
          val isThin = (v \ "@isThin").text.toBoolean
          val id = (v \ "@id").text.toInt

          exprMap(id) = Variable(Identifier.empty, isThin)
          identMap(id) = ident
        }

        case f @ <filler><fillerIdent>{fillerIdentContent}</fillerIdent><bdryIdent>{bdryIdentContent}</bdryIdent></filler> => {

          val fillerIdent = rawIdentifierFromXML(fillerIdentContent)
          val bdryIdent = rawIdentifierFromXML(bdryIdentContent)
          val bdryIsThin = (f \ "@bdryIsThin").text.toBoolean
          val bdryId = (f \ "@bdryId").text.toInt
          val id = (f \ "@id").text.toInt

          val filler = Filler(Identifier.empty, Identifier.empty, bdryIsThin)

          exprMap(id) = filler
          identMap(id) = fillerIdent
          
          exprMap(bdryId) = filler.MyBoundary
          identMap(bdryId) = bdryIdent
        }
      }

    def resolveIdent(rawIdent : RawIdentifier) : Identifier = 
      Identifier(
        rawIdent.tokens map {
          case RawLiteral(lit) => LiteralToken(lit)
          case RawReference(id) => ExpressionToken(exprMap(id.toInt))
        }
      )

    def readEnvironment(node : xml.Node) : SimpleEnvironmentNode =
      node match {
        case g @ <group>{groupContent @ _*}</group> => {
          val chldrn = groupContent map (readEnvironment(_))
          val name = (g \ "@name").text

          val envNode = new SimpleEnvironmentNode(GroupElement(name))
          envNode.children ++= chldrn

          envNode
        }
        case <expression>{exprContent}</expression> => {
          val expr = XmlSerializable.cellSerializable[String].fromXML(exprContent)
          val el = ExpressionElement(NCell.cellIsNCell(expr map (id => exprMap(id.toInt))))
          new SimpleEnvironmentNode(el)
        }
      }

    node match {
      case defXml @ <definition><expressions>{expressionContent @ _*}</expressions><environment>{environmentContent}</environment></definition> => {

        def intToOpt(i : Int) : Option[Int] = if (i < 0) None else Some(i)

        val stabilityLevel : Option[Int] = intToOpt((defXml \ "@slevel").text.toInt)
        val invertibilityLevel : Option[Int] = intToOpt((defXml \ "@ilevel").text.toInt)
        val unicityLevel : Option[Int] = intToOpt((defXml \ "@ulevel").text.toInt)

        // This should fill the expression map with information
        expressionContent foreach (readExpression(_))

        // Before we read in the shapes in the environment, we need to
        // fix up the identifiers
        exprMap foreach {
          case (id, v @ Variable(_, _)) => {
            v.ident = resolveIdent(identMap(id))
          }
          case (id, f @ Filler(_, _, _)) => {
            f.ident = resolveIdent(identMap(id))
          }
          case (id, b : Filler#Boundary) => {
            b.interior.bdryIdent = resolveIdent(identMap(id))
          }
        }

        val envRoot = readEnvironment(environmentContent)
        new Definition(envRoot, stabilityLevel, invertibilityLevel, unicityLevel)
      }
    }
  }

}
