/*
 *
 * TODO fix rotation implementation
 * TODO raycasting functionalities
 * TODO rastering using raycasting
 * TODO reflections using normals and raycasting
 *
 */
package controllers
import StandardFunctions.{Quit, Start, Update}
import models.Hierarchy
import utils.Logging._
object App {
  var running = true //als dit false is stopt de applicatie
  val hierarchy: Hierarchy = new Hierarchy() //bevat de assets en andere info

  def main(args: Array[String]) {
    s"${Console.GREEN}Program started".log
    hierarchy.time = 0
    Start()

    while (running) {
      if (!hierarchy.run) {
        running = false
        Quit()
        s"${Console.GREEN}Program ended!".log
      } else {
        hierarchy.time += 1
        try {
          Update()
        } catch {
          case _: java.awt.IllegalComponentStateException =>
            hierarchy.run = false
            s"${Console.YELLOW}Program ended (probably because of the window being closed.)".log
          case e: Throwable =>
            hierarchy.run = false
            s"${Console.RED}Program ended: ${e.getMessage}".log
        }
      }
    }
  }
}
