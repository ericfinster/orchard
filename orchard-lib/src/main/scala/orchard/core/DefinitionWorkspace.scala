/**
  * DefinitonWorkspace.scala - A workspace implementation for definitons
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Buffer

import Environment._

abstract class DefinitionWorkspace extends Workspace with SequentialContextEnvironment {

  class DefinitionWorksheet(seed : NCell[Polarity[Seq[Int]]]) 
      extends Worksheet(seed) {

    type CellType = DefinitionWorksheetCell

    def newCell(itm : Polarity[Seq[Int]]) = new DefinitionWorksheetCell(itm)
    def extract(cell : DefinitionWorksheetCell) = new DefinitionWorksheet(cell.toNCell)
    
    // This is where we look it up in the context
    def getExpression(idx : Polarity[Seq[Int]]) : Option[Expression[Seq[Int]]] =
      idx match {
        case Neutral(seq) => get(seq)
        case _ => None
      }

    class DefinitionWorksheetCell(itm : Polarity[Seq[Int]]) extends WorksheetCell(itm)

  }

  // def createDefinition(expr : NCell[Expression]) : Option[Definition] = {
  //   val framework = Framework(expr)

  //   framework.glob(None, None)
  //   val deps = framework.dependencies(environment)

  //   val defnEnv = Buffer.empty ++ environment
  //   defnEnv filter (e => deps.containsId(e.value.id))

  //   val defn =
  //     new Definition(
  //       name,
  //       stabilityLevel,
  //       invertibilityLevel,
  //       unicityLevel,
  //       defnEnv,
  //       expr.value)

  //   Some(defn)
  // }

}
