/**
  * SimpleFramework.scala - An implementation of the ExpressionFramework trait
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.WeakHashMap

class SimpleFramework(seed : NCell[Option[Expression]]) 
    extends AbstractMutableComplex[Option[Expression]](seed) with ExpressionFramework[Option[Expression]] {

  type CellType = SimpleFrameworkCell

  def newCell(item : Option[Expression]) = new SimpleFrameworkCell(item)

  class SimpleFrameworkCell(var item : Option[Expression])
      extends AbstractMutableCell with ExpressionFrameworkCell {

    def exprItem = item

    def frameworkToXML(env : Map[String, String]) = {
    }

  }

  // Now, I want to put an xml writing routine here which takes into account
  // a given environment.

  // How does this work?
}
