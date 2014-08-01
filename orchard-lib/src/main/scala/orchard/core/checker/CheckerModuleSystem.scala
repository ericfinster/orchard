/**
  * CheckerModuleSystem.scala - Server side checker module system
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

trait CheckerModuleSystem extends ModuleSystem { thisChecker : Checker =>

  type NodeType = CheckerNode
  type ContainerNodeType = CheckerContainerNode
  type ExpressionNodeType = CheckerExpressionNode
  type ModuleNodeType = CheckerModuleNode
  type ImportNodeType = CheckerImportNode
  type ParameterNodeType = CheckerParameterNode
  type DefinitionNodeType = CheckerDefinitionNode

  abstract class CheckerNode extends Node {
    def name : String
  }

  abstract class CheckerContainerNode extends CheckerNode with ContainerNode
  abstract class CheckerExpressionNode extends CheckerNode with ExpressionNode

  class CheckerModuleNode(val moduleId : String) extends CheckerContainerNode with ModuleNode {
    def insertEntryAt(entry : ModuleEntry, index : Int) : Unit = ()
    def name = moduleId
  }

  class CheckerImportNode(val name : String, val module : Module, val isOpen : Boolean) extends CheckerContainerNode with ImportNode {

    def moduleName = module.node.name
    
  }

  class CheckerParameterNode(val identifier : Identifier, val shell : Shell, val isThin : Boolean) extends CheckerExpressionNode with ParameterNode {

    def name : String = identifier.expand

    val variableExpression : Variable = Variable(identifier, shell, isThin)

  }

  class CheckerDefinitionNode(val identifier : Identifier, val nook : Nook) extends CheckerExpressionNode with DefinitionNode {

    def name : String = identifier.expand

    val fillerExpression : Filler = Filler(identifier, nook)
    val boundaryExpression : Filler#BoundaryExpr = fillerExpression.Boundary

  }

}
