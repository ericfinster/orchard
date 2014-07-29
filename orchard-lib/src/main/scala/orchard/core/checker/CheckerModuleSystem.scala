/**
  * CheckerModuleSystem.scala - Server side checker module system
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

trait CheckerModuleSystem extends ModuleSystem {

  type NodeType = CheckerNode
  type ModuleNodeType = CheckerModuleNode
  type ParameterNodeType = CheckerParameterNode
  type DefinitionNodeType = CheckerDefinitionNode
  type ImportNodeType = CheckerImportNode

  class CheckerNode extends Node {
    def name : String = "Unknown"
  }

  class CheckerModuleNode(val moduleId : String) extends CheckerNode with ModuleNode {
    def insertEntryAt(entry : ModuleEntry, index : Int) : Unit = ()
    override def name = moduleId
  }

  class CheckerParameterNode extends CheckerNode with ParameterNode
  class CheckerDefinitionNode extends CheckerNode with DefinitionNode
  class CheckerImportNode extends CheckerNode with ImportNode

}
