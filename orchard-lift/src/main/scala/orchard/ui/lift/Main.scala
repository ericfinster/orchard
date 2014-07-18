/**
  * Main.scala - A Class for doing some debugging
  * 
  * @author Eric Finster
  * @version 0.1 
  */

import orchard.core.svg._
import orchard.core.cell._
import orchard.core.complex._

object Main extends App {

  val panel = new SimpleSVGPanel(new SimpleMutableComplex(Example.Psi), 2)

  panel.render

  println("Rendering Finished.")

}
