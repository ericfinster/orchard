/**
  * CheckerModuleSystem.scala - Server side checker module system
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

trait CheckerModuleSystem extends ModuleSystem { thisChecker : Checker =>

  type NodeType = CheckerNode
  type ModuleNodeType = CheckerModuleNode
  type ParameterNodeType = CheckerParameterNode
  type DefinitionNodeType = CheckerDefinitionNode
  type ImportNodeType = CheckerImportNode

  abstract class CheckerNode extends Node {
    def name : String
  }

  class CheckerModuleNode(val moduleId : String) extends CheckerNode with ModuleNode {
    def insertEntryAt(entry : ModuleEntry, index : Int) : Unit = ()
    def name = moduleId
  }

  class CheckerParameterNode(val identifier : Identifier, val shell : Shell, val isThin : Boolean) extends CheckerNode with ParameterNode {

    def name : String = identifier.expand

    def variableExpression : Variable = Variable(identifier, shell, isThin)

  }

  class CheckerDefinitionNode extends CheckerNode with DefinitionNode {

    def name : String = ???

  }

  class CheckerImportNode extends CheckerNode with ImportNode {

    def name : String = ???

  }

}
