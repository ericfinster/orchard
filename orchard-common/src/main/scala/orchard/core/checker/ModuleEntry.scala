/**
  * ModuleEntry.scala - The data structure describing modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import orchard.core.util.ErrorM._

// Here's what you could do:  the descriptions could have like 
// insertion events, which could be called from the zipper routines
// when insertions and removals take place.  But maybe this is a 
// bit complicated.  Anyway, something to think about ...

trait Description { def name : String ; var address : Vector[Int] }
trait ModuleDescription extends Description { def insertEntryAt(e : ModuleEntry, i : Int) }
trait ParameterDescription extends Description
trait DefinitionDescription extends Description

sealed trait ModuleEntry { def desc : Description }
case class Module(val desc : ModuleDescription, val entries : Vector[ModuleEntry]) extends ModuleEntry
case class Parameter(val desc : ParameterDescription) extends ModuleEntry
case class Definition(val desc : DefinitionDescription) extends ModuleEntry

case class ModuleContext(desc : ModuleDescription, left : Vector[ModuleEntry], right : Vector[ModuleEntry]) {

  def insertAfter(entry : ModuleEntry) : ModuleContext = 
    ModuleContext(desc, left, entry +: right)

  def insertBefore(entry : ModuleEntry) : ModuleContext = 
    ModuleContext(desc, left :+ entry, right)

}


case class ModuleZipper(val focus : ModuleEntry, val context : List[ModuleContext]) {

  def zipOnce : Error[ModuleZipper] =
    context match {
      case Nil => fail("Zip failed: emtpy context.")
      case ModuleContext(desc, left, right) :: cs =>
        success(ModuleZipper(Module(desc, left ++ Vector(focus) ++ right), cs))
    }

  def zip : ModuleEntry = 
    context match {
      case Nil => focus
      case ModuleContext(desc, left, right) :: cs => 
        ModuleZipper(Module(desc, left ++ Vector(focus) ++ right), cs).zip
    }

  def toAddress : Vector[Int] = 
    context match {
      case Nil => Vector.empty
      case ModuleContext(desc, left, right) :: cs => 
        ModuleZipper(Module(desc, left ++ Vector(focus) ++ right), cs).toAddress :+ left.length
    }

  def insertAfter(entry : ModuleEntry) : Error[ModuleZipper] =
    context match {
      case Nil => fail("Cannot insert: no module open")
      case c :: cs => success(ModuleZipper(focus, c.insertAfter(entry) :: cs))
    }

  def insertBefore(entry : ModuleEntry) : Error[ModuleZipper] =
    context match {
      case Nil => fail("Cannot insert: no module open")
      case c :: cs => success(ModuleZipper(focus, c.insertBefore(entry) :: cs))
    }

  // Warning! The semantics of this method are a bit different from the two preceding it.
  def insertAt(entry : ModuleEntry, i : Int) : Error[ModuleZipper] = 
    focus match {
      case m : Module => {
        val (left, right) = m.entries.splitAt(i)

        // Now, we should have the insertion routine pass to all of the
        // *right* entries and update their addresses.

        // Then we can pass the module itself directly to an insertion routine
        // on the module description so that all of the stateful updates are 
        // hidden by this purely functional routine.

        m.desc.insertEntryAt(entry, i)
        
        val zip = ModuleZipper(m.copy(entries = left ++ Vector(entry) ++ right), context)

        // Now, for the values in the correct range, update all the addresses...

        for {
          k <- Range(left.length, m.entries.length + 1)
          child <- zip.visitEntry(k)
        } {
          child.foreachWithAddress {
            case (addr, desc) => {
              println("Setting address of " ++ desc.name ++ " to " ++ addr.toString)
              desc.address = addr
            }
          }
        }

        success(zip)
      }
      case _ => fail("Focused entry cannot have children")
    }
  
  def appendEntry(entry : ModuleEntry) : Error[ModuleZipper] =
    focus match {
      case m : Module => 
        success(ModuleZipper(m.copy(entries = m.entries :+ entry), context))
      case _ => fail("Focused entry cannot have children.")
    }

  def preprendEntry(entry : ModuleEntry) : Error[ModuleZipper] =
    focus match {
      case m : Module => 
        success(ModuleZipper(m.copy(entries = entry +: m.entries), context))
      case _ => fail("Focused entry cannot have children.")
    }

  def visitEntry(i : Int) : Error[ModuleZipper] = 
    focus match {
      case m : Module => 
        if (i < 0 || i > m.entries.length - 1)
          fail("No entry at index " ++ i.toString)
        else {
          val (left, rightPlus) = m.entries.splitAt(i)
          success(ModuleZipper(rightPlus.head, ModuleContext(m.desc, left, rightPlus.tail) :: context))
        }
      case _ => fail("Focused module has no children.")
    }

  def seek(addr : Vector[Int]) : Error[ModuleZipper] = 
    if (addr.length <= 0) {
      success(this)
    } else {
      for {
        entry <- visitEntry(addr.head)
        res <- entry.seek(addr.tail)
      } yield res
    }

//   def firstChild : Error[ModuleZipper] = {
//     focus match {
//       case m : Module => 
//         if (m.entries.length > 0) {
//           ???
//         } else fail("Focused entry does not have children.")
//       case _ => fail("Focused entry does not have children.")
//     }
//   }

  def foreachWithAddress(f : Function2[Vector[Int], Description, Unit]) : Unit =
    focus match {
      case m : Module => {
        for {
          i <- Range(0, m.entries.length)
        } {
          println("Visiting index " ++ i.toString ++ " on module " ++ m.desc.name)
          visitEntry(i).get.foreachWithAddress(f)
        }

        f(toAddress, m.desc)
      }
      case p : Parameter => f(toAddress, p.desc)
      case d : Definition => f(toAddress, d.desc)
    }

  def focusedModule : Error[Module] =
    focus match {
      case m : Module => success(m)
      case _ => fail("Focused entry is not a module.")
    }
}
