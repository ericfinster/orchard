/**
  * ModuleSystem.scala - Common traits and classes for server and client module systems
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import orchard.core.util.ErrorM._

trait ModuleSystem {

  type NodeType <: Node
  type ContainerNodeType <: NodeType with ContainerNode
  type ExpressionNodeType <: NodeType with ExpressionNode
  type ModuleNodeType <: ContainerNodeType with ModuleNode
  type ImportNodeType <: ContainerNodeType with ImportNode
  type ParameterNodeType <: ExpressionNodeType with ParameterNode
  type DefinitionNodeType <: ExpressionNodeType with DefinitionNode

  def rootZipper : Error[ModuleZipper]

  trait Node {
    thisNode : NodeType =>

    def name : String 
    def nodeType : String

    var address : Vector[Int] = Vector.empty

    def parentNode : Error[Node] = 
      for {
        root <- rootZipper
        zipper <- root.seek(address.init)
      } yield zipper.focus.node

    def qualifiedName : String = 
      parentNode match {
        case Right(node) => node.qualifiedName ++ "." ++ name
        case Left(_) => name
      }

  }

  trait ContainerNode extends Node { thisNode : ContainerNodeType => }
  trait ExpressionNode extends Node { thisNode : ExpressionNodeType => }

  trait ModuleNode extends ContainerNode { 
    thisNode : ModuleNodeType =>

    def nodeType = "moduleNode"
    def insertEntryAt(me : ModuleEntry, i : Int) : Unit

  }

  trait ImportNode extends ContainerNode {
    thisNode : ImportNodeType =>

    def nodeType = "importNode"

    def moduleName : String
    def isOpen : Boolean

  }


  trait ParameterNode extends ExpressionNode { 
    thisNode : ParameterNodeType =>

    def nodeType = "parameterNode"

  }

  trait DefinitionNode extends ExpressionNode {
    thisNode : DefinitionNodeType =>

    def nodeType = "definitionNode"

  }

  object Node {

    import orchard.core.util._

    trait SummaryNodeGenerator {
      def createModule(name : String) : ModuleNodeType
      def createImport(name : String, moduleName : String, isOpen : Boolean) : ImportNodeType
      def createParameter(name : String) : ParameterNodeType
      def createDefinition(name : String) : DefinitionNodeType
    }

    implicit def nodeIsReadable[P](implicit gen : SummaryNodeGenerator) : JsonReadable[NodeType, P] =
      new JsonReadable[NodeType, P] {
        def read(x : P, reader : JsonReader[P]): NodeType = {
          val nodeType = reader.readString(reader.readObjectField(x, "type"))
          val name = reader.readString(reader.readObjectField(x, "name"))

          nodeType match {
            case "moduleNode" => {
              gen.createModule(name)
            }
            case "importNode" => {
              val moduleName = reader.readString(reader.readObjectField(x, "moduleName"))
              val isOpen = reader.readBoolean(reader.readObjectField(x, "isOpen"))

              gen.createImport(name, moduleName, isOpen)
            }
            case "parameterNode" => {
              gen.createParameter(name)
            }
            case "definitionNode" => {
              gen.createDefinition(name)
            }
          }
        }
      }

    implicit def nodeIsWritable[P] : JsonWritable[Node, P] =
      new JsonWritable[Node, P] {
        def write(node : Node, writer : JsonWriter[P]) : P = {
          node match {
            case i : ImportNode => {
              writer.writeObject(
                "type" -> writer.writeString(i.nodeType),
                "name" -> writer.writeString(i.name),
                "moduleName" -> writer.writeString(i.moduleName),
                "isOpen" -> writer.writeBoolean(i.isOpen)
              )
            } 
            case _ => {
              writer.writeObject(
                "type" -> writer.writeString(node.nodeType),
                "name" -> writer.writeString(node.name)
              )
            }
          }
        }
      }
    
  }

  //============================================================================================
  // MODULE ENTRIES
  //

  sealed trait ModuleEntry { def node : NodeType } 

  case class Module(val moduleNode : ModuleNodeType, val entries : Vector[ModuleEntry]) extends ModuleEntry {
    def node = moduleNode

    override def toString =
      (entries map (_.toString)).mkString("Begin module: " ++ moduleNode.name ++ "\n", "\n", "\nEnd module: " ++ moduleNode.name)

  }

  case class Import(val importNode : ImportNodeType, val entries : Vector[ModuleEntry]) extends ModuleEntry {
    def node = importNode
  }

  case class Parameter(val parameterNode : ParameterNodeType) extends ModuleEntry {
    def node = parameterNode

    override def toString = 
      "Parameter: " ++ parameterNode.name

  }

  case class Definition(val definitionNode : DefinitionNodeType) extends ModuleEntry { 
    def node = definitionNode
  }

  object ModuleEntry {

    import orchard.core.util._

    implicit def moduleEntryIsReadable[P](implicit gen : Node.SummaryNodeGenerator) : JsonReadable[ModuleEntry, P] =
      new JsonReadable[ModuleEntry, P] {
        def read(x : P, reader : JsonReader[P]) : ModuleEntry = {
          val nodeReader = implicitly[JsonReadable[NodeType, P]]
          val entryReader = implicitly[JsonReadable[Vector[ModuleEntry], P]]

          val nodeType = reader.readString(reader.readObjectField(x, "type"))


          nodeType match {
            case "module" => {
              val entries = entryReader.read(reader.readObjectField(x, "entries"), reader)
              val node = nodeReader.read(reader.readObjectField(x, "node"), reader).asInstanceOf[ModuleNodeType]
              Module(node, entries)
            }
            case "import" => {
              val entries = entryReader.read(reader.readObjectField(x, "entries"), reader)
              val node = nodeReader.read(reader.readObjectField(x, "node"), reader).asInstanceOf[ImportNodeType]
              Import(node, entries)
            }
            case "parameter" => {
              val node = nodeReader.read(reader.readObjectField(x, "node"), reader).asInstanceOf[ParameterNodeType]
              Parameter(node)
            }
            case "definition" => {
              val node = nodeReader.read(reader.readObjectField(x, "node"), reader).asInstanceOf[DefinitionNodeType]
              Definition(node)
            }
          }
        }
      }

    implicit def moduleEntryIsWritable[P] : JsonWritable[ModuleEntry, P] =
      new JsonWritable[ModuleEntry, P] {
        def write(entry : ModuleEntry, writer : JsonWriter[P]) : P = {
          val nodeWriter = implicitly[JsonWritable[Node, P]]
          val entryWriter = implicitly[JsonWritable[Vector[ModuleEntry], P]]

          entry match {
            case m : Module => {
              writer.writeObject(
                "type" -> writer.writeString("module"),
                "node" -> nodeWriter.write(m.node, writer),
                "entries" -> entryWriter.write(m.entries, writer)
              )
            }
            case i : Import => {
              writer.writeObject(
                "type" -> writer.writeString("import"),
                "node" -> nodeWriter.write(i.node, writer),
                "entries" -> entryWriter.write(i.entries, writer)
              )
            }
            case p : Parameter => {
              writer.writeObject(
                "type" -> writer.writeString("parameter"),
                "node" -> nodeWriter.write(p.node, writer)
              )
            }
            case d : Definition => {
              writer.writeObject(
                "type" -> writer.writeString("definition"),
                "node" -> nodeWriter.write(d.node, writer)
              )
            }
          }
        }
      }
    
  }


  //============================================================================================
  // MODULE CONTEXTS AND ZIPPERS
  //

  case class ModuleContext(node : ModuleNodeType, left : Vector[ModuleEntry], right : Vector[ModuleEntry]) {

    def insertAfter(entry : ModuleEntry) : ModuleContext =
      ModuleContext(node, left, entry +: right)

    def insertBefore(entry : ModuleEntry) : ModuleContext =
      ModuleContext(node, left :+ entry, right)

  }


  case class ModuleZipper(val focus : ModuleEntry, val context : List[ModuleContext]) {

    def zipOnce : Error[ModuleZipper] =
      context match {
        case Nil => fail("Zip failed: emtpy context.")
        case ModuleContext(node, left, right) :: cs =>
          success(ModuleZipper(Module(node, left ++ Vector(focus) ++ right), cs))
      }

    def zip : ModuleEntry =
      context match {
        case Nil => focus
        case ModuleContext(node, left, right) :: cs =>
          ModuleZipper(Module(node, left ++ Vector(focus) ++ right), cs).zip
      }

    def collectLefts : Vector[ModuleEntry] = 
      context match {
        case Nil => Vector.empty
        case ModuleContext(node, left, right) :: cs => {
          ModuleZipper(Module(node, left ++ Vector(focus) ++ right), cs).collectLefts ++ left
        }
      }

    def toAddress : Vector[Int] =
      context match {
        case Nil => Vector.empty
        case ModuleContext(node, left, right) :: cs =>
          ModuleZipper(Module(node, left ++ Vector(focus) ++ right), cs).toAddress :+ left.length
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

          m.node.insertEntryAt(entry, i)
          
          val zip = ModuleZipper(m.copy(entries = left ++ Vector(entry) ++ right), context)

          // Now, for the values in the correct range, update all the addresses...

          for {
            k <- Range(left.length, m.entries.length + 1)
            child <- zip.visitEntry(k)
          } {
            child.foreachWithAddress {
              case (addr, node) => {
                node.address = addr
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
            success(ModuleZipper(rightPlus.head, ModuleContext(m.node, left, rightPlus.tail) :: context))
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

    def foreachWithAddress(f : Function2[Vector[Int], NodeType, Unit]) : Unit =
      focus match {
        case m : Module => {
          for {
            i <- Range(0, m.entries.length)
          } {
            println("Visiting index " ++ i.toString ++ " on module " ++ m.node.name)
            visitEntry(i).get.foreachWithAddress(f)
          }

          f(toAddress, m.node)
        }
        case p : Parameter => f(toAddress, p.node)
        case d : Definition => f(toAddress, d.node)
        case i : Import => {
          // This is not right, but we will have to think about it as the semantics improve
          f(toAddress, i.node)
        }
      }

    def focusedModule : Error[Module] =
      focus match {
        case m : Module => success(m)
        case _ => fail("Focused entry is not a module.")
      }
  }
  

}
