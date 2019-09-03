package models

import scala.swing.MainFrame

//window
class SFrame (h : Hierarchy) extends MainFrame {
  override def closeOperation () : Unit = {
    h.Quit()
  }
}
