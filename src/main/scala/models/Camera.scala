package models

import java.awt.Point

class Camera (h_ : Hierarchy) {
  var h: Hierarchy = h_
  //var position = new Vector3(0,0,0)
  //var rotation = new Vector3(0,0,0)
  //var clipNear = 0.1f
  //var clipFar = 100.0f
  var FoV = 40
  //var camMatrix = new Matrix(3,4)

  def WorldToScreen (v : Vector3): Point = {
    val sx = h.frame.size.width/2
    val sy = h.frame.size.height/2
    def r: Float = if(v.z != 0) 0f else 0.001f

    val y1 : Float = (FoV/(v.z + r)) * v.x
    val y2 : Float = (FoV/(v.z + r)) * v.y
    new Point(y1.toInt+sx, y2.toInt+sy)
  }
}
