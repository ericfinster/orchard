/**
  * SimpleFramework.scala - An implementation of the ExpressionFramework trait
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

class SimpleFramework(seed : NCell[Option[Expression]]) 
    extends MutableComplex[Option[Expression]] with ExpressionFramework[Option[Expression]] {

  type CellType = SimpleFrameworkCell

  def newCell(item : Option[Expression]) = new SimpleFrameworkCell(item)

  populateComplex(seed)

  class SimpleFrameworkCell(var item : Option[Expression])
      extends MutableCell with ExpressionFrameworkCell {

    def exprItem = item

    def frameworkToXML(env : Map[String, String]) = {
    }
  }

  // Now, I want to put an xml writing routine here which takes into account
  // a given environment.

  // How does this work?
}
