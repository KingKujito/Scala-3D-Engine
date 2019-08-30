package controllers

import java.awt.{MouseInfo, Point}

import models.Mesh.Triangle

import scala.collection.mutable.ListBuffer
import models._

object StandardFunctions {
  var running = true	//als dit false is stopt de applicatie
  val hierarchy : Hierarchy = new Hierarchy() //bevat de assets en andere info

  def Start () : Unit = {
    //window.open()
    for (i <- 0 until hierarchy.assets.length) {
      hierarchy.assets(i).transform.setParent(hierarchy.worldTrans)
    }
    hierarchy.frame.visible = true
    hierarchy.Start()
  }

  def Update () : Unit = {
    //val xx = (MouseInfo.getPointerInfo().getLocation().x - hierarchy.frame.locationOnScreen.x)/ (hierarchy.frame.size.width/hierarchy.pScreenX)
    //val yy = (MouseInfo.getPointerInfo().getLocation().y - hierarchy.frame.locationOnScreen.y)/ (hierarchy.frame.size.height/hierarchy.pScreenY)
    hierarchy.canvas.triangles = new ListBuffer[Triangle]()
    val xx = MouseInfo.getPointerInfo.getLocation.x - hierarchy.frame.locationOnScreen.x
    val yy = MouseInfo.getPointerInfo.getLocation.y - hierarchy.frame.locationOnScreen.y
    hierarchy.MousePos = new Point(xx-10,yy-30)
    hierarchy.Update()
  }

  def Quit () : Unit = {
    running = false
    hierarchy.OnQuit()
    //window.close()
    hierarchy.frame.close()
  }

}
