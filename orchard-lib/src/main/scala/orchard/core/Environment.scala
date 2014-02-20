/**
  * Environment.scala - Operations in an environment
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.Buffer

trait Environment {

  type EnvironmentSeqType <: Seq[NCell[Expression]]

  def environment : EnvironmentSeqType

  def environmentContains(id : String) : Boolean = {
    environment exists (expr => expr.value.id == id)
  }

  def getFromEnvironment(id : String) : Option[NCell[Expression]] = {
    environment find (expr => expr.value.id == id)
  }

  def environmentVariables : Seq[NCell[Expression]] = 
    environment filter (expr => {
      expr.value match {
        case Variable(_, _) => true
        case _ => false
      }
    })

  // Please look at these routines again ... I know they could be done much better ...

  def dependencies(framework : SimpleFramework) : Map[String, NCell[Expression]] = {
    val deps = HashMap.empty[String, NCell[Expression]]
    collectDependencies(framework, deps)
    deps
  }

  def freeVariables(framework : SimpleFramework) : Map[String, NCell[Expression]] = {
    val freeVars = HashMap.empty[String, NCell[Expression]]
    collectFreeVars(framework, freeVars)
    freeVars
  }

  def collectDependencies(framework : SimpleFramework, deps : Map[String, NCell[Expression]]) : Unit = {
    // Save the top item so that the top cell does not appear as a dependency
    val topItem = framework.topCell.item
    framework.topCell.item = None

    framework.variables foreach (v => {
      if (! deps.isDefinedAt(v.value.id)) { deps(v.value.id) = v }
    })

    framework.fillers foreach (f => {
      if (! deps.isDefinedAt(f.value.id)) { deps(f.value.id) = f }
    })

    // This is the complicated step
    framework.fillerFaces foreach (ff => {
      val face = ff.value.asInstanceOf[FillerFace]
      val fillerExpr = getFromEnvironment(face.filler).get
      val fillerFramework = SimpleFramework(fillerExpr)

      if (! deps.isDefinedAt(ff.value.id)) { deps(ff.value.id) = ff }

      fillerFramework.topCell.target.get.foreachCell (cell => {
        cell.item foreach (e => {
          if (e.id == face.id) { cell.item = None }
        })
      })

      collectDependencies(fillerFramework, deps)
    })

    framework.topCell.item = topItem
  }

  def collectFreeVars(framework : SimpleFramework, freeVars : HashMap[String, NCell[Expression]]) : Unit = {
    // Grab the obvious free variables from the framework
    framework.variables foreach (v => {
      if (! freeVars.isDefinedAt(v.value.id)) { freeVars(v.value.id) = v }
    })

    // Hmm. We need to avoid hitting the same filler face again.  We can do this
    // as we did before by blanking out the top cell and it's filler

    // We don't care about the fillers, only the faces
    framework.fillerFaces foreach (ff => {
      val face = ff.value.asInstanceOf[FillerFace]
      val fillerExpr = getFromEnvironment(face.filler).get
      val fillerFramework = SimpleFramework(fillerExpr)

      fillerFramework.topCell.target.get.foreachCell (cell => {
        cell.item foreach (e => {
          if (e.id == face.id) { cell.item = None }
        })
      })

      collectFreeVars(fillerFramework, freeVars)
    })
  }
}
