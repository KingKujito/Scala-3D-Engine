package models

import java.awt.geom.Rectangle2D
import java.awt.{BasicStroke, Color, Graphics2D}
import Mesh._
class Renderer (asset : Asset) {
  var is3D : Boolean = false
  var isWireframe : Boolean = false
  def Draw (g : Graphics2D) : Unit = {

  }
}

object Renderer {
  class RectRend (asset : Asset) extends Renderer (asset : Asset) {
    is3D = false
    isWireframe = false
    override def Draw (g : Graphics2D) : Unit = {
      if (asset.active) {
        val transform = asset.transform
        g.setColor(Color.white)
        g.fill(new Rectangle2D.Double(transform.position.x, transform.position.y, transform.scale.x, transform.scale.y))
      }
    }
  }

  class CubeRend (asset : Asset) extends Renderer (asset : Asset) {
    is3D = true
    isWireframe = true
    override def Draw (g : Graphics2D) : Unit = {
      if (asset.active) {
        val c = Cube(asset)
        c.Draw()
        for (i <- c.vertices.indices) {
          val xy1 = asset.h.cam.WorldToScreen(c.vertices(i).vertex(0))
          val xy2 = asset.h.cam.WorldToScreen(c.vertices(i).vertex(1))
          val xy3 = asset.h.cam.WorldToScreen(c.vertices(i).vertex(2))
          //val sx = asset.h.frame.size.width/2
          //val sy = asset.h.frame.size.height/2
          g.setStroke(new BasicStroke(3))
          g.setColor(Color.red)
          g.drawLine(xy1.x, xy1.y, xy2.x, xy2.y)
          g.setColor(Color.blue)
          g.drawLine(xy3.x, xy3.y, xy2.x, xy2.y)
          g.setColor(Color.yellow)
          g.drawLine(xy1.x, xy1.y, xy3.x, xy3.y)
        }
      }
    }
  }

  class PyrRend (asset : Asset) extends Renderer (asset : Asset) {
    is3D = true
    isWireframe = true
    override def Draw (g : Graphics2D) : Unit = {
      if (asset.active) {
        val c = Pyramid(asset)
        c.Draw()
        for (i <- c.vertices.indices) {
          val xy1 = asset.h.cam.WorldToScreen(c.vertices(i).vertex(0))
          val xy2 = asset.h.cam.WorldToScreen(c.vertices(i).vertex(1))
          val xy3 = asset.h.cam.WorldToScreen(c.vertices(i).vertex(2))
          //val sx = asset.h.frame.size.width/2
          //val sy = asset.h.frame.size.height/2
          g.setStroke(new BasicStroke(3))
          g.setColor(Color.red)
          g.drawLine(xy1.x, xy1.y, xy2.x, xy2.y)
          g.setColor(Color.blue)
          g.drawLine(xy3.x, xy3.y, xy2.x, xy2.y)
          g.setColor(Color.yellow)
          g.drawLine(xy1.x, xy1.y, xy3.x, xy3.y)
        }
      }
    }
  }

  class AppleRend (asset : Asset) extends Renderer (asset : Asset) {
    is3D = true
    isWireframe = false
    override def Draw (g : Graphics2D) : Unit = {
      if (asset.active) {
        val c = AppleShape(asset)
        c.Draw()
        for (i <- c.vertices.indices) {
          asset.h.canvas.triangles += c.vertices(i)
        }
      }
    }
  }

  class CubeFillRend (asset : Asset) extends Renderer (asset : Asset) {
    is3D = true
    isWireframe = false
    override def Draw (g : Graphics2D) : Unit = {
      if (asset.active) {
        val c = CubeFillShape(asset)
        c.Draw()
        for (i <- c.vertices.indices) {
          asset.h.canvas.triangles += c.vertices(i)
        }
      }
    }
  }

  class PyrFillRend (asset : Asset) extends Renderer (asset : Asset) {
    is3D = true
    isWireframe = false
    override def Draw (g : Graphics2D) : Unit = {
      if (asset.active) {
        val c = PyrFillShape(asset)
        c.Draw()
        for (i <- c.vertices.indices) {
          asset.h.canvas.triangles += c.vertices(i)
        }
      }
    }
  }
}
