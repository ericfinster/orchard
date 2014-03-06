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

  }

}
