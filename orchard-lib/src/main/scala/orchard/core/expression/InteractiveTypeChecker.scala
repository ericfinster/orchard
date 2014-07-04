/**
  * InteractiveTypeChecker.scala - A type checker supporting user interaction
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

abstract class InteractiveTypeChecker 
    extends TypeChecker
    with WorkspaceModule
    with WorksheetModule {

  type WorkspaceType <: Workspace

  def getActiveWorkspace : CheckerResult[WorkspaceType]
  def setActiveWorkspace(wksp : WorkspaceType) : CheckerResult[Unit]


}
