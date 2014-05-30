/**
  * ModuleVariable.scala - A variable appearing in a module
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

trait ModuleVariable extends ModuleEntry { thisVariable : ModuleEnvironment#ModuleVariable =>

  def varExpr : Variable

  override def name = varExpr.id

}
