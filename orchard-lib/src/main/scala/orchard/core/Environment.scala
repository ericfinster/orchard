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

object Environment {

  implicit class EnvironmentOps(env : Seq[NCell[Expression]]) {

    def contains(id : String) : Boolean = 
      env exists (expr => expr.value.id == id)

    def lookup(id : String) : Option[NCell[Expression]] = 
      env find (expr => expr.value.id == id)

    def vars : Seq[NCell[Expression]] = 
      env filter (expr => {
        expr.value match {
          case Variable(_, _) => true
          case _ => false
        }
      })

    def fills : Seq[NCell[Expression]] = 
      env filter (expr => {
        expr.value match {
          case Filler(_) => true
          case UnicityFiller(_) => true
          case _ => false
        }
      })

    def deps(id : String) : Option[Seq[NCell[Expression]]] = 
      for {
        expr <- lookup(id)
      } yield {
        val ds = Buffer.empty[NCell[Expression]]
        deps(id, ds)
        env filter (expr => ds.contains(expr.value.id))
      }

    def deps(id : String, ds : Buffer[NCell[Expression]]) : Unit = 
      for {
        expr <- lookup(id)
      } {
        expr.value match {
          case FillerFace(_, filler, _) => deps(filler, ds)
          case _ => {

            // As it stands, objects cannot have dependencies.  Is this okay?
            // You may run into problems later with stability ....
            if (expr.dimension.toInt > 0) {
              val framework = SimpleFramework(expr)
              val base = framework(expr.dimension.toInt - 1)

              base foreachCell (cell => {
                cell.item match {
                  case Some(FillerFace(ident, filler, _)) => {
                    if (filler != id) {
                      env.deps(ident.toString, ds)
                      if (! ds.contains(ident.toString)) { ds += env.lookup(ident.toString).get }
                    } 
                  }
                  case Some(ex @ _) => {
                    env.deps(ex.id, ds)
                    if (! ds.contains(ex.id)) { ds += env.lookup(ex.id).get }
                  }
                }
              })
            }
          }
        }
      }

    def depVars(id : String) : Option[Seq[NCell[Expression]]] = 
      for { ds <- deps(id) } yield ds.vars
  }

}
