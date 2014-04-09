/**
  * Template.scala - Lift templates
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import orchard.core.cell.NCell
import orchard.core.util.XmlSerializable

// Need filling parameters to be saved for the template ...

class Template(val root : GroupNode) {

  def name : String = root.name

}

object Template {

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

  def environmentToXML(node : EnvironmentNode) : xml.Node = 
    node match {
      case g @ GroupNode(name) => {
        <group name={name}>{ g.children map (environmentToXML(_)) }</group>
      }
      case ExpressionNode(expr) => {
        <expression>{
          XmlSerializable.cellSerializable[String].toXML(expr map (_.hashCode.toString))
        }</expression>
      }
    }

  def templateToXML(template : Template) : xml.Node = {
    val exprXML = template.root.toExprSeq map (expressionToXML(_))
    val envXML = environmentToXML(template.root)
    <template name={template.name}><expressions>{exprXML}</expressions><environment>{envXML}</environment></template>
  }

  def fromXML(node : xml.Node) : Template = {
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

          println("Finished reading variable: " ++ ident.toString)
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

          println("Finished reading filler: " ++ fillerIdent.toString)
        }
      }

    def resolveIdent(rawIdent : RawIdentifier) : Identifier = 
      Identifier(
        rawIdent.tokens map {
          case RawLiteral(lit) => LiteralToken(lit)
          case RawReference(id) => ExpressionToken(exprMap(id.toInt))
        }
      )

    def readEnvironment(node : xml.Node) : EnvironmentNode = 
      node match {
        case g @ <group>{groupContent @ _*}</group> => {
          val chldrn = groupContent map (readEnvironment(_))
          val name = (g \ "@name").text

          val envNode = GroupNode(name)
          envNode.children ++= chldrn

          envNode
        }
        case <expression>{exprContent}</expression> => {
          println("Reading expression ...")
          val expr = XmlSerializable.cellSerializable[String].fromXML(exprContent)
          ExpressionNode(NCell.cellIsNCell(expr map (id => exprMap(id.toInt))))
        }
      }

    node match {
      case <template><expressions>{expressionContent @ _*}</expressions><environment>{environmentContent}</environment></template> => {

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

        new Template(readEnvironment(environmentContent).asInstanceOf[GroupNode])
      }
    }
  }

}
