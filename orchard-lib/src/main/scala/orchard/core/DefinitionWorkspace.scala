/**
  * DefinitonWorkspace.scala - A workspace implementation for definitons
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Buffer

import Environment._

abstract class DefinitionWorkspace extends Workspace {

  def createDefinition(expr : NCell[Expression]) : Option[Definition] = {
    val framework = Framework(expr)

    framework.glob(None, None)
    val deps = framework.dependencies(environment)

    val defnEnv = Buffer.empty ++ environment
    defnEnv filter (e => deps.containsId(e.value.id))

    val defn =
      new Definition(
        name,
        stabilityLevel,
        invertibilityLevel,
        unicityLevel,
        defnEnv,
        expr.value)

    Some(defn)
  }

}
