package models

import java.awt.{Color, Graphics2D, Polygon}
import java.awt.geom.Rectangle2D

import models.Mesh.Triangle

import scala.collection.mutable.ListBuffer
import scala.swing.{Component, Dimension}

//rendering canvas
class Canvas(pixelData : Array[Array[Color]], scr : Dimension, h : Hierarchy) extends Component {
  preferredSize = scr
  val row: Int = pixelData.length-1		//hoeveel pixels breed
  val col: Int = pixelData(0).length-1	//hoeveel pixels hoog
  var triangles = new ListBuffer[Triangle]() //de polygonen die we gaan renderen

  override def paintComponent(g : Graphics2D) {
    val d = size
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING,
      java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g.setColor(Color.black)
    g.fillRect(0,0, d.width, d.height)
    for (i <- 0 to row; l <- 0 to col) {
      g.setColor(pixelData(i)(l))
      g.fill(new Rectangle2D.Double(i*d.width/row , l*d.height/col , d.width/row + 1, d.height/col + 1))
    }

    g.setColor(Color.black)
    g.fillRect(0,0, h.frame.size.width, h.frame.size.height)

    val assts = h.assets.sortBy(_.transform.position.z).toArray
    for (i <- assts.length-1 to 0 by -1) {
      if (assts(i).active) {
        g.setColor(Color.green)
        assts(i).renderer.Draw(g)
      }
    }

    if (triangles.nonEmpty) {
      val trngls = triangles.toList.sortWith( _.dist > _.dist ).toArray
      for (i <- trngls.indices) {
        g.setColor(trngls(i).color)
        val xy1 = h.cam.WorldToScreen(trngls(i).vertex(0))
        val xy2 = h.cam.WorldToScreen(trngls(i).vertex(1))
        val xy3 = h.cam.WorldToScreen(trngls(i).vertex(2))
        g.fill(new Polygon(Array(xy1.x, xy2.x, xy3.x), Array(xy1.y, xy2.y, xy3.y), 3))

        //draw wire frames
        //g.setStroke(new BasicStroke(1))
        //	g.setColor(Color.white)
        //	g.drawLine(xy1.x, xy1.y, xy2.x, xy2.y)
      }
    }

    h.PostDraw(g)
  }
}
