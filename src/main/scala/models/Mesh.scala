package models

import java.awt.Color

import models.Mesh._

abstract class Mesh[A <: VertexBase] {
  val asset: Asset
  var vertices : Array[A] = init()
  var vertexBasePositions : Array[A] = init()

  //declare vertices in init
  def init() : Array[A]
  def Draw () : Unit = {
    for (i <- vertices.indices; l <- vertices(i).vertex.indices) {
      //applyScaleAndPos(vertices(i).vertex(l), vertexBasePositions(i).vertex(l),asset.transform)
      applyScale(vertices(i).vertex(l), vertexBasePositions(i).vertex(l), asset.transform)
      applyRotation(vertices(i).vertex(l), asset.transform)
      applyPos(vertices(i).vertex(l), vertexBasePositions(i).vertex(l),asset.transform)
      vertices(i).Update()
      vertices(i).dist = new Global().Distance(vertices(i).position, Vector3(0,0,0))
    }
  }
}

object Mesh {
  private def applyScaleAndPos(vertex : Vector3, oVertex : Vector3, transform: Transform): Unit = {
    vertex.x = oVertex.x *transform.scale.x + transform.position.x
    vertex.y = oVertex.y *transform.scale.y + transform.position.y
    vertex.z = oVertex.z *transform.scale.z + transform.position.z
  }

  private def applyScale(vertex : Vector3, oVertex : Vector3, transform: Transform): Unit = {
    vertex.x = oVertex.x *transform.scale.x
    vertex.y = oVertex.y *transform.scale.y
    vertex.z = oVertex.z *transform.scale.z
  }

  private def applyPos(vertex : Vector3, oVertex : Vector3, transform: Transform): Unit = {
    vertex.x += transform.position.x
    vertex.y += transform.position.y
    vertex.z += transform.position.z
  }

  //x' = x cos a - y sin a
  //y' = x sin a + y cos a
  private def applyRotation(vertex : Vector3, transform: Transform): Unit = {
    vertex.x = (vertex.x * Math.cos(transform.rotation.x) - vertex.y * Math.sin(transform.rotation.x)).toFloat
    vertex.y = (vertex.y * Math.cos(transform.rotation.y) - vertex.x * Math.sin(transform.rotation.y)).toFloat
    vertex.z = (vertex.z * Math.cos(transform.rotation.z) - vertex.x * Math.sin(transform.rotation.z)).toFloat
  }

  class Matrix(r : Int, c : Int) {
    var matrix = new Array[Float](r)(c)
  }

  class Mat4() {
    var matrix = new Array[Float](4)(4)
  }

  trait VertexBase {
    var vertex : Array[Vector3] = new Array[Vector3](3)
    val v1 : Vector3
    val v2 : Vector3
    val v3 : Vector3
    var position = Vector3( (v1.x+v2.x+v3.x)/3, (v1.y+v2.y+v3.y)/3, (v1.z+v2.z+v3.z)/3 )
    var dist = 0.0f

    def Update () : Unit = {
      position = Vector3( (vertex(0).x+vertex(1).x+vertex(2).x)/3, (vertex(0).y+vertex(1).y+vertex(2).y)/3, (vertex(0).z+vertex(1).z+vertex(2).z)/3 )
    }
  }
  
  case class Vertex (v1 : Vector3, v2 : Vector3, v3 : Vector3) extends VertexBase {
    //var vertex = points
    vertex(0) = v1
    vertex(1) = v2
    vertex(2) = v3
  }

  case class Triangle (v1 : Vector3, v2 : Vector3, v3 : Vector3, c : Color = Color.pink) extends VertexBase {
    var color: Color = c
    //var vertex = points
    vertex(0) = v1
    vertex(1) = v2
    vertex(2) = v3
  }

  case class Cube (asset : Asset) extends Mesh[Vertex] {
    def init(): Array[Vertex] =
      Array(
      //front
        Vertex(Vector3(-1, 1, -1), Vector3(1, 1, -1), Vector3(1, -1, -1)),
        Vertex(Vector3(-1, 1, -1), Vector3(-1, -1, -1), Vector3(1, -1, -1)),
      //back
        Vertex(Vector3(-1, 1, 1), Vector3(1, 1, 1), Vector3(1, -1, 1)),
        Vertex(Vector3(-1, 1, 1), Vector3(-1, -1, 1), Vector3(1, -1, 1)),
      //top
        Vertex(Vector3(-1, 1, -1), Vector3(1, 1, -1), Vector3(1, 1, 1)),
        Vertex(Vector3(-1, 1, -1), Vector3(-1, 1, 1), Vector3(1, 1, 1)),
      //bottom
        Vertex(Vector3(-1, -1, -1), Vector3(1, -1, -1), Vector3(1, -1, 1)),
        Vertex(Vector3(-1, -1, -1), Vector3(-1, -1, 1), Vector3(1, -1, 1)),
      //left
        Vertex(Vector3(-1, 1, -1), Vector3(-1, 1, 1), Vector3(-1, -1, -1)),
        Vertex(Vector3(-1, 1, -1), Vector3(-1, -1, 1), Vector3(-1, -1, -1)),
      //right
        Vertex(Vector3(1, 1, -1), Vector3(1, 1, 1), Vector3(1, -1, -1)),
        Vertex(Vector3(1, 1, -1), Vector3(1, -1, 1), Vector3(1, -1, -1))
      )
  }

  case class Cube2 (asset : Asset) extends Mesh[Vertex] {
    def init(): Array[Vertex] = Array(
      //front
      Vertex(Vector3(-2, 1, -1), Vector3(0, 1, -1), Vector3(0, -1, -1)),
      Vertex(Vector3(-2, 1, -1), Vector3(-2, -1, -1), Vector3(0, -1, -1)),
      //back
      Vertex(Vector3(-1, 1, 1), Vector3(1, 1, 1), Vector3(1, -1, 1)),
      Vertex(Vector3(-1, 1, 1), Vector3(-1, -1, 1), Vector3(1, -1, 1)),
      //top
      Vertex(Vector3(-2, 1, -1), Vector3(0, 1, -1), Vector3(1, 1, 1)),
      Vertex(Vector3(-2, 1, -1), Vector3(-1, 1, 1), Vector3(1, 1, 1)),
      //bottom
      Vertex(Vector3(-2, -1, -1), Vector3(0, -1, -1), Vector3(1, -1, 1)),
      Vertex(Vector3(-2, -1, -1), Vector3(-1, -1, 1), Vector3(1, -1, 1)),
      //left
      Vertex(Vector3(-2, 1, -1), Vector3(-1, 1, 1), Vector3(-2, -1, -1)),
      Vertex(Vector3(-2, 1, -1), Vector3(-1, -1, 1), Vector3(-2, -1, -1)),
      //right
      Vertex(Vector3(0, 1, -1), Vector3(1, 1, 1), Vector3(0, -1, -1)),
      Vertex(Vector3(0, 1, -1), Vector3(1, -1, 1), Vector3(0, -1, -1))
    )
  }

  case class Pyramid (asset : Asset) extends Mesh[Vertex] {
    def init(): Array[Vertex] = Array(
      //pyramid
      Vertex(Vector3(0, 1, 0), Vector3(0, -1, -1), Vector3(1, -1, 1)),
      Vertex(Vector3(0, 1, 0), Vector3(0, -1, -1), Vector3(-1, -1, 1)),
      Vertex(Vector3(0, 1, 0), Vector3(1, -1, 1), Vector3(-1, -1, 1)),
      Vertex(Vector3(0, -1, -1), Vector3(1, -1, 1), Vector3(-1, -1, 1))
    )
  }

  case class AppleShape (asset : Asset) extends Mesh[Triangle] {
    def init(): Array[Triangle] = Array(
      //voorkant
      Triangle(Vector3(0, 1, 0), Vector3(0, 0, 1), Vector3(-1, 0, 0), new Color(255, 124, 122)), //linksboven
      Triangle(Vector3(0, 1, 0), Vector3(0, 0, 1), Vector3(1, 0, 0), new Color(227, 27, 16)), //rechtsboven
      Triangle(Vector3(0, -1, 0), Vector3(0, 0, 1), Vector3(-1, 0, 0), new Color(227, 27, 16)), //linksonder
      Triangle(Vector3(0, -1, 0), Vector3(0, 0, 1), Vector3(1, 0, 0), new Color(177, 0, 0)), //rechtsonder
      //achterkant
      Triangle(Vector3(0, 1, 0), Vector3(0, 0, -1), Vector3(-1, 0, 0), new Color(227, 27, 16)), //linksboven
      Triangle(Vector3(0, 1, 0), Vector3(0, 0, -1), Vector3(1, 0, 0), new Color(177, 0, 0)), //rechtsboven
      Triangle(Vector3(0, -1, 0), Vector3(0, 0, -1), Vector3(-1, 0, 0), new Color(177, 0, 0)), //linksonder
      Triangle(Vector3(0, -1, 0), Vector3(0, 0, -1), Vector3(1, 0, 0), new Color(104, 0, 0)), //rechtsonder
      //blaadje
      Triangle(Vector3(0, 0, 0), Vector3(0.6f, 0.7f, 0.3f), Vector3(0.6f, 0.7f, -0.3f), new Color(16, 204, 0)),
      Triangle(Vector3(0.8f, 0.3f, 0), Vector3(0.6f, 0.7f, 0.3f), Vector3(0.6f, 0.7f, -0.3f), new Color(11, 113, 0))
    )
  }

  case class CubeFillShape (asset : Asset) extends Mesh[Triangle] {
    def init(): Array[Triangle] = Array(
      //front
      Triangle(Vector3(-1, 1, -1), Vector3(1, 1, -1), Vector3(1, -1, -1), new Color(180, 180, 180)),
      Triangle(Vector3(-1, 1, -1), Vector3(-1, -1, -1), Vector3(1, -1, -1), new Color(180, 180, 180)),
      //back
      Triangle(Vector3(-1, 1, 1), Vector3(1, 1, 1), Vector3(1, -1, 1), new Color(110, 110, 110)),
      Triangle(Vector3(-1, 1, 1), Vector3(-1, -1, 1), Vector3(1, -1, 1), new Color(110, 110, 110)),
      //top
      Triangle(Vector3(-1, 1, -1), Vector3(1, 1, -1), Vector3(1, 1, 1), new Color(240, 240, 240)),
      Triangle(Vector3(-1, 1, -1), Vector3(-1, 1, 1), Vector3(1, 1, 1), new Color(240, 240, 240)),
      //bottom
      Triangle(Vector3(-1, -1, -1), Vector3(1, -1, -1), Vector3(1, -1, 1), new Color(80, 80, 80)),
      Triangle(Vector3(-1, -1, -1), Vector3(-1, -1, 1), Vector3(1, -1, 1), new Color(80, 80, 80)),
      //left
      Triangle(Vector3(-1, 1, -1), Vector3(-1, 1, 1), Vector3(-1, -1, -1), new Color(140, 140, 140)),
      Triangle(Vector3(-1, 1, -1), Vector3(-1, -1, 1), Vector3(-1, -1, -1), new Color(140, 140, 140)),
      //right
      Triangle(Vector3(1, 1, -1), Vector3(1, 1, 1), Vector3(1, -1, -1), new Color(140, 140, 140)),
      Triangle(Vector3(1, 1, -1), Vector3(1, -1, 1), Vector3(1, -1, -1), new Color(140, 140, 140))
    )
  }

  case class PyrFillShape (asset : Asset) extends Mesh[Triangle] {
    def init(): Array[Triangle] = Array(
      //pyramid
      Triangle(Vector3(0, 1, 0), Vector3(0, -1, -1), Vector3(1, -1, 1), Color.green),
      Triangle(Vector3(0, 1, 0), Vector3(0, -1, -1), Vector3(-1, -1, 1), Color.red),
      Triangle(Vector3(0, 1, 0), Vector3(1, -1, 1), Vector3(-1, -1, 1), Color.yellow),
      Triangle(Vector3(0, -1, -1), Vector3(1, -1, 1), Vector3(-1, -1, 1), Color.blue)
    )
  }
}
