/**
  * Environment.scala - Operations in an environment
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.language.implicitConversions
import scala.collection.mutable.Buffer

object Environment {

  // implicit class Shape(val ncell : NCell[Seq[Int]]) 

  // implicit class Context(val ctxt : Buffer[Expression]) {

  //   def containsId(id : String) : Boolean = 
  //     ctxt exists (expr => expr.id == id)

  //   def findById(id : String) : Option[Expression] = 
  //     ctxt find (expr => expr.id == id)

  //   // And this is wrong, since if you check while extending, you could
  //   // end up in a state with a dirty context when you have to add two 
  //   // expressions in a row
  //   def extendWith(expr : Expression) = 
  //     if (containsId(expr.id)) {
  //       throw new IllegalArgumentException("Duplicate identifier in context.")
  //     } else {
  //       if (expr.id == "") {
  //         throw new IllegalArgumentException("Refusing to except emtpy identifier.")
  //       } else {
  //         ctxt += expr
  //       }
  //     }

  //   // This is essentially useless, since it forgets the position of the variables
  //   // which we will basically always need
  //   def variables : Seq[Expression] = 
  //     ctxt filter {
  //       case Variable(_, _, _) => true
  //       case _ => false
  //     }

  // }

  // implicit def shapeToNCell(shape : Shape) : NCell[Seq[Int]] = shape.ncell
  // implicit def contextToBuf(ctx : Context) : Seq[Expression] = ctx.ctxt

}
