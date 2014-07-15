/**
  * ModuleEntry.scala - Entries which hold commands to pass to the typechecker
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import javafx.scene.{control => jfxsc}

import orchard.core.typechecker._
import Statement._

abstract class ModuleItem extends jfxsc.TreeItem[ModuleItem] {

  setValue(this)

  def statement : FreeM[Unit] 

}

class ModuleDeclaration extends ModuleItem {

  def statement : FreeM[Unit] = ???

}

class ParameterDeclaration(name : String)  extends ModuleItem {

  def statement : FreeM[Unit] = 
    paramDec(name)

}

class LiftDeclaration(name : String) extends ModuleItem {

  def statement : FreeM[Unit] = 
    liftDec(name)

}
