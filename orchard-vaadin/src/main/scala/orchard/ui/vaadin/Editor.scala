/**
  * Editor.scala - Vaadin implementation 
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.vaadin

import vaadin.scala._

class Editor extends UI {
  content = Button("Final answer?", Notification.show("Hello, world!"))
}
