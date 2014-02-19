/**
  * SubstitutionWorkspace.scala - A workspace implementation for substitutions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Buffer

abstract class SubstitutionWorkspace extends Workspace {

  def shell : SimpleFramework
  def defn : Definition

  val goals = Buffer.empty[NCell[Expression]]

  // Initialize all the goals ....
  defn.environmentVariables foreach (v => {
    val shellClone = shell.clone
    shellClone.stablyAppend(new SimpleFramework(v map (Some(_))))
    goals += shellClone.toExpressionCell
  })

}
