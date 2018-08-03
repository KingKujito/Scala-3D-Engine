//REFINED SCALA APP TEMPLATE
/*
* To do:
*	-rotation application
*		x' = x cos a - y sin a
*		y' = x sin a + y cos a
*	-texture mapping?
*	-input management
*	-test app
*/


//-----------------------IMPORTS----------------------------//
//-----------------------IMPORTS----------------------------//
//-----------------------IMPORTS----------------------------//
import scala.swing._
import scala.collection.mutable.ListBuffer
import java.awt.{ Color, Graphics2D, Rectangle, BasicStroke, Point, PointerInfo, MouseInfo, Font, Polygon }
import java.awt.geom._
//-----------------------APPLICATION STUFF----------------------------//
//-----------------------APPLICATION STUFF----------------------------//
//-----------------------APPLICATION STUFF----------------------------//
object Appl {
//-----------------------VARS----------------------------//
//-----------------------VARS----------------------------//
//-----------------------VARS----------------------------//
	var running = true	//als dit false is stopt de applicatie
	val hierarchy : Hierarchy = new Hierarchy() //bevat de assets en andere info

//-----------------------MAIN----------------------------//
//-----------------------MAIN----------------------------//
//-----------------------MAIN----------------------------//
	def main (args : Array[String]) {
		
		println("program started")
		hierarchy.time = 0
		Start()

		while (running) {
			if(!hierarchy.run) {
				Quit()
			}
			hierarchy.time += 1
			Update()
		}
	}

//-----------------------STANDARD FUNCTIONS----------------------------//
//-----------------------STANDARD FUNCTIONS----------------------------//
//-----------------------STANDARD FUNCTIONS----------------------------//
	def Start () : Unit = {
		//window.open()
		for (i <- 0 to hierarchy.assets.length-1) {
			hierarchy.assets(i).transform.setParent(hierarchy.worldTrans)
		}
		hierarchy.frame.visible = true
		hierarchy.Start()
	}

	def Update () : Unit = {
		//val xx = (MouseInfo.getPointerInfo().getLocation().x - hierarchy.frame.locationOnScreen.x)/ (hierarchy.frame.size.width/hierarchy.pScreenX)
		//val yy = (MouseInfo.getPointerInfo().getLocation().y - hierarchy.frame.locationOnScreen.y)/ (hierarchy.frame.size.height/hierarchy.pScreenY)
		hierarchy.canvas.triangles = new ListBuffer[Triangle]()
		val xx = (MouseInfo.getPointerInfo().getLocation().x - hierarchy.frame.locationOnScreen.x)
		val yy = (MouseInfo.getPointerInfo().getLocation().y - hierarchy.frame.locationOnScreen.y)
		hierarchy.MousePos = new Point(xx-10,yy-30);
		hierarchy.Update()
	}

	def Quit () : Unit = {
		running = false
		hierarchy.OnQuit()
		//window.close()
		hierarchy.frame.close()
		println("program ended")
	}

}

//-----------------------GLOBAL FUNCTIONS AND VARS----------------------------//
//-----------------------GLOBAL FUNCTIONS AND VARS----------------------------//
//-----------------------GLOBAL FUNCTIONS AND VARS----------------------------//
class Global () {
//-----------------------MATHS----------------------------//
//-----------------------MATHS----------------------------//
//-----------------------MATHS----------------------------//

	//afstand in 3d ruimte
	def Distance (v1 : Vector3, v2 : Vector3) : Float = {
		return math.sqrt(math.pow(v2.x-v1.x , 2)+math.pow(v2.y-v1.y , 2)+math.pow(v2.z-v1.z , 2)).toFloat
	}

	//willekeurige float tussen min en max
	def RandRange (min : Float, max : Float) : Float = {
		return (math.random * (max-min) + min).toFloat
	}

	//willekeurige int tussen min en max
	def RandRange (min : Int, max : Int) : Int = {
		return (math.random * (max-min) + min).toInt
	}

//-----------------------ASSET FUNCTIONS----------------------------//
//-----------------------ASSET FUNCTIONS----------------------------//
//-----------------------ASSET FUNCTIONS----------------------------//
	//geef een list van alle assets met naam s in lijst l
	def findAssets (s : String, l : List[Asset]) : List[Asset] = {
		var buff = new ListBuffer[Asset]()
		for (a <- l if(a.name != null && a.name == s)) {
			buff += a
		}
		return buff.toList
	}

	//vind de eerste asset met naam s in lijst l
	def find (s : String, l : List[Asset]) : Asset = {
		var l2: List[Asset] = findAssets(s,l)
		return l2(0)
	}

	//geef een list van alle assets met tag s in lijst l
	def findAssetsWithTag (s : String, l : List[Asset]) : List[Asset] = {
		var buff = new ListBuffer[Asset]()
		for (a <- l if(a.tag != null && a.tag == s)) {
			buff += a
		}
		return buff.toList
	}

	//vind de eerste asset met tag s in lijst l
	def findWithTag (s : String, l : List[Asset]) : Asset = {
		var l2: List[Asset] = findAssetsWithTag(s,l)
		return l2(0)
	}
}


//-----------------------BASIC ASSET----------------------------//
//-----------------------BASIC ASSET----------------------------//
//-----------------------BASIC ASSET----------------------------//
class Asset (name_ : String, tag_ : String, active_ : Boolean, h_ : Hierarchy) {
	var h = h_
	var name = name_
	var tag = tag_
	var active = active_
	var renderer : Renderer = new Renderer(this);
	val id = new Global().RandRange(0,999999)
	var transform = new Transform(id, null, Vector3(0,0,0), Vector3(0,0,0), Vector3(0,0,0), null, this)
}

//-----------------------TRANSFORM----------------------------//
//-----------------------TRANSFORM----------------------------//
//-----------------------TRANSFORM----------------------------//
case class Vector3 (var x : Float, var y : Float, var z : Float)	//een datatype die drie floats opslaat

class Transform (id : Int, parent_ : Transform, position_ : Vector3, rotation_ : Vector3, scale_  : Vector3, child_ : Map[Int, Transform], asset_ : Asset) {
	val ID = id
	var parent = parent_		//waar zit deze transform aan vast
	var position = position_	//positie in de wereld
	var rotation = rotation_	//rotatie in de wereld
	var scale = scale_			//grootte in de wereld
	var child = child_			//kinder transforms [id, transform]
	var asset = asset_			//asset waar de transform aan vast zit

	//geef een list met alle kinder transforms
	def children () : List[Transform] = {
		if (child == null || child.values.toArray.length < 1) {
			return null
		} else {
			return child.values.toList
		}
	}

	//koppel een ouder
	def setParent (t : Transform) : Unit = {
		if(t == null) {
			if (parent != null) {
				parent.child -= ID
			}
			parent = null
		} else {
		parent = t
			if (t.child == null || t.child.values.toArray.length < 1) {
				t.child = Map[Int, Transform](ID -> this)
			} else {
				t.child += (ID -> this)
			}
		}
	}

	//beweeg
	def Translate (w : Vector3) : Unit = {
		this.position.x += w.x
		this.position.y += w.y
		this.position.z += w.z
		//pas ook toe op kinderen
		for(a <- children) {
			a.Translate(w)
		}
	}

	//vergroot of verklein
	def Grow (w : Vector3) : Unit = {
		this.scale.x += w.x
		this.scale.y += w.y
		this.scale.z += w.z
		for(a <- children) {
			a.Translate(w)
		}
	}

	//draai
	def Rotate (w : Vector3) : Unit = {
		this.rotation.x += w.x
		this.rotation.y += w.y
		this.rotation.z += w.z
		for(a <- children) {
			a.Translate(w)
		}
	}

	//geef gegevens weer in locale waarde
	def localPosition () : Vector3 = {
		if (parent != null) {
			val w : Vector3 = Vector3( this.position.x - parent.position.x, this.position.y - parent.position.y, this.position.z - parent.position.z )
			return w
		} else {
			return this.position
		}
	}

	def localRotation () : Vector3 = {
		if (parent != null) {
			val w : Vector3 = Vector3( this.rotation.x - parent.rotation.x, this.rotation.y - parent.rotation.y, this.rotation.z - parent.rotation.z )
			return w
		} else {
			return this.rotation
		}
	}

	def localScale () : Vector3 = {
		if (parent != null) {
			val w : Vector3 = Vector3( this.scale.x - parent.scale.x, this.scale.y - parent.scale.y, this.scale.z - parent.scale.z )
			return w
		} else {
			return this.scale
		}
	}

	def setLocalPosition (v : Vector3) : Unit = {
		val w : Vector3 = Vector3(parent.position.x + v.x, parent.position.y + v.y, parent.position.z + v.z)
		this.position = w
	}

	def setLocalRotation (v : Vector3) : Unit = {
		val w : Vector3 = Vector3(parent.rotation.x + v.x, parent.rotation.y + v.y, parent.rotation.z + v.z)
		this.rotation = w
	}

	def setLocalScale (v : Vector3) : Unit = {
		val w : Vector3 = Vector3(parent.localScale().x + v.x, parent.localScale().y + v.y, parent.localScale().z + v.z)
		this.scale = w 
	}


	def setPosition (v : Vector3) : Unit = {
		if (children() != null) {
			val a = children().toArray
			for (i <- 0 to a.length - 1) {
				val v2 = new Vector3 ( v.x + a(i).localPosition.x , v.y + a(i).localPosition.y , v.z + a(i).localPosition.z )
				a(i).setPosition( v2 )
			}
		}
		this.position = v
	}

	def setRotation (v : Vector3) : Unit = {
		if (children() != null) {
			val a = children().toArray
			for (i <- 0 to a.length - 1) {
				val v2 = new Vector3 ( v.x + a(i).localRotation.x , v.y + a(i).localRotation.y , v.z + a(i).localRotation.z )
				a(i).setRotation( v2 )
			}
		}
		this.rotation = v
	}

	def setScale (v : Vector3) : Unit = {
		if (children() != null) {
			val a = children().toArray
			for (i <- 0 to a.length - 1) {
				val v2 = new Vector3 ( a(i).localScale.x + v.x , a(i).localScale.y + v.y , a(i).localScale.z + v.z )
				a(i).setScale( v2 )
			}
		}
		this.scale = v
	}
}


//-----------------------RENDERER FOR ASSETS----------------------------//
//-----------------------RENDERER FOR ASSETS----------------------------//
//-----------------------RENDERER FOR ASSETS----------------------------//
class Camera (h_ : Hierarchy) {
	var h = h_
	//var position = new Vector3(0,0,0)
	//var rotation = new Vector3(0,0,0)
	//var clipNear = 0.1f
	//var clipFar = 100.0f
	var FoV = 40
	//var camMatrix = new Matrix(3,4)

	def WorldToScreen (v : Vector3): Point = {
		val sx = h.frame.size.width/2
		val sy = h.frame.size.height/2
		var r = 0.001f
		if (v.z != 0) {
			r = 0
		}
		var y1 : Float = (FoV/(v.z+r))*v.x
		var y2 : Float = (FoV/(v.z+r))*v.y
		return new Point(y1.toInt+sx, y2.toInt+sy)
	}
}

class Renderer (asset : Asset) {
	var is3D : Boolean = false
	var isWireframe : Boolean = false
	def Draw (g : Graphics2D) : Unit = {
		
	}
}

class rectRend (asset : Asset) extends Renderer (asset : Asset) {
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

class cubeRend (asset : Asset) extends Renderer (asset : Asset) {
	is3D = true
	isWireframe = true
	override def Draw (g : Graphics2D) : Unit = {
		if (asset.active) {
			val c = new cube(asset)
			c.Draw()
			for (i <- 0 to c.vertices.length-1) {
				val xy1 = asset.h.cam.WorldToScreen(c.vertices(i).vertex(0))
				val xy2 = asset.h.cam.WorldToScreen(c.vertices(i).vertex(1))
				val xy3 = asset.h.cam.WorldToScreen(c.vertices(i).vertex(2))
				val sx = asset.h.frame.size.width/2
				val sy = asset.h.frame.size.height/2
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

class pyrRend (asset : Asset) extends Renderer (asset : Asset) {
	is3D = true
	isWireframe = true
	override def Draw (g : Graphics2D) : Unit = {
		if (asset.active) {
			val c = new pyramid(asset)
			c.Draw()
			for (i <- 0 to c.vertices.length-1) {
				val xy1 = asset.h.cam.WorldToScreen(c.vertices(i).vertex(0))
				val xy2 = asset.h.cam.WorldToScreen(c.vertices(i).vertex(1))
				val xy3 = asset.h.cam.WorldToScreen(c.vertices(i).vertex(2))
				val sx = asset.h.frame.size.width/2
				val sy = asset.h.frame.size.height/2
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

class appleRend (asset : Asset) extends Renderer (asset : Asset) {
	is3D = true
	isWireframe = false
	override def Draw (g : Graphics2D) : Unit = {
		if (asset.active) {
			val c = new appleShape(asset)
			c.Draw()
			for (i <- 0 to c.vertices2.length-1) {
				asset.h.canvas.triangles += c.vertices2(i)
			}
		}
	}
}

class cubeFillRend (asset : Asset) extends Renderer (asset : Asset) {
	is3D = true
	isWireframe = false
	override def Draw (g : Graphics2D) : Unit = {
		if (asset.active) {
			val c = new cubeFillShape(asset)
			c.Draw()
			for (i <- 0 to c.vertices2.length-1) {
				asset.h.canvas.triangles += c.vertices2(i)
			}
		}
	}
}

class pyrFillRend (asset : Asset) extends Renderer (asset : Asset) {
	is3D = true
	isWireframe = false
	override def Draw (g : Graphics2D) : Unit = {
		if (asset.active) {
			val c = new pyrFillShape(asset)
			c.Draw()
			for (i <- 0 to c.vertices2.length-1) {
				asset.h.canvas.triangles += c.vertices2(i)
			}
		}
	}
}
//-----------------------MESHES----------------------------//
//-----------------------MESHES----------------------------//
//-----------------------MESHES----------------------------//
class Matrix(r : Int, c : Int) {
	var matrix = new Array[Float](r)(c)
}

class mat4() {
	var matrix = new Array[Float](4)(4)
}

class Vertex (v1 : Vector3, v2 : Vector3, v3 : Vector3) {
	var vertex = new Array[Vector3](3)
	//var vertex = points
	vertex(0) = v1
	vertex(1) = v2
	vertex(2) = v3
}

class Triangle (v1 : Vector3, v2 : Vector3, v3 : Vector3, c : Color) extends Vertex (v1 : Vector3, v2 : Vector3, v3 : Vector3) {
	vertex = new Array[Vector3](3)
	var color = c
	//var vertex = points
	vertex(0) = v1
	vertex(1) = v2
	vertex(2) = v3
	var position = new Vector3( (v1.x+v2.x+v3.x)/3, (v1.y+v2.y+v3.y)/3, (v1.z+v2.z+v3.z)/3 )
	var dist = 0.0f

	def Update () : Unit = {
		position = new Vector3( (vertex(0).x+vertex(1).x+vertex(2).x)/3, (vertex(0).y+vertex(1).y+vertex(2).y)/3, (vertex(0).z+vertex(1).z+vertex(2).z)/3 )
	}
}


abstract class Mesh () {
	var vertices : Array[Vertex]
	def Draw () : Unit = {
		//do something
	}
}

class cube (asset : Asset) extends Mesh () {
	var vertices = new Array[Vertex](12)
	//front
		vertices(0) = new Vertex( new Vector3(-1,1,-1), new Vector3(1,1,-1), new Vector3(1,-1,-1))
		vertices(1) = new Vertex( new Vector3(-1,1,-1), new Vector3(-1,-1,-1), new Vector3(1,-1,-1))
		//back
		vertices(2) = new Vertex( new Vector3(-1,1,1), new Vector3(1,1,1), new Vector3(1,-1,1))
		vertices(3) = new Vertex( new Vector3(-1,1,1), new Vector3(-1,-1,1), new Vector3(1,-1,1))
		//top
		vertices(4) = new Vertex( new Vector3(-1,1,-1), new Vector3(1,1,-1), new Vector3(1,1,1))
		vertices(5) = new Vertex( new Vector3(-1,1,-1), new Vector3(-1,1,1), new Vector3(1,1,1))
		//bottom
		vertices(6) = new Vertex( new Vector3(-1,-1,-1), new Vector3(1,-1,-1), new Vector3(1,-1,1))
		vertices(7) = new Vertex( new Vector3(-1,-1,-1), new Vector3(-1,-1,1), new Vector3(1,-1,1))
		//left
		vertices(8) = new Vertex( new Vector3(-1,1,-1), new Vector3(-1,1,1), new Vector3(-1,-1,-1))
		vertices(9) = new Vertex( new Vector3(-1,1,-1), new Vector3(-1,-1,1), new Vector3(-1,-1,-1))
		//right
		vertices(10) = new Vertex( new Vector3(1,1,-1), new Vector3(1,1,1), new Vector3(1,-1,-1))
		vertices(11) = new Vertex( new Vector3(1,1,-1), new Vector3(1,-1,1), new Vector3(1,-1,-1))

	override def Draw () : Unit = {
		val transform = asset.transform
		for (i <- 0 to vertices.length-1; l <- 0 to vertices(i).vertex.length-1) {
			vertices(i).vertex(l).x = vertices(i).vertex(l).x*transform.scale.x + transform.position.x
			vertices(i).vertex(l).y = vertices(i).vertex(l).y*transform.scale.y + transform.position.y
			vertices(i).vertex(l).z = vertices(i).vertex(l).z*transform.scale.z + transform.position.z
		}
	}
}

class cube2 (asset : Asset) extends Mesh () {
	var vertices = new Array[Vertex](10)
	//front
		vertices(0) = new Vertex( new Vector3(-2,1,-1), new Vector3(0,1,-1), new Vector3(0,-1,-1))
		vertices(1) = new Vertex( new Vector3(-2,1,-1), new Vector3(-2,-1,-1), new Vector3(0,-1,-1))
		//back
		vertices(2) = new Vertex( new Vector3(-1,1,1), new Vector3(1,1,1), new Vector3(1,-1,1))
		vertices(3) = new Vertex( new Vector3(-1,1,1), new Vector3(-1,-1,1), new Vector3(1,-1,1))
		//top
		vertices(4) = new Vertex( new Vector3(-2,1,-1), new Vector3(0,1,-1), new Vector3(1,1,1))
		vertices(5) = new Vertex( new Vector3(-2,1,-1), new Vector3(-1,1,1), new Vector3(1,1,1))
		//bottom
		vertices(6) = new Vertex( new Vector3(-2,-1,-1), new Vector3(0,-1,-1), new Vector3(1,-1,1))
		vertices(7) = new Vertex( new Vector3(-2,-1,-1), new Vector3(-1,-1,1), new Vector3(1,-1,1))
		//left
		vertices(8) = new Vertex( new Vector3(-2,1,-1), new Vector3(-1,1,1), new Vector3(-2,-1,-1))
		vertices(9) = new Vertex( new Vector3(-2,1,-1), new Vector3(-1,-1,1), new Vector3(-2,-1,-1))
		//right
		vertices(8) = new Vertex( new Vector3(0,1,-1), new Vector3(1,1,1), new Vector3(0,-1,-1))
		vertices(9) = new Vertex( new Vector3(0,1,-1), new Vector3(1,-1,1), new Vector3(0,-1,-1))

	override def Draw () : Unit = {
		val transform = asset.transform
		for (i <- 0 to vertices.length-1; l <- 0 to vertices(i).vertex.length-1) {
			vertices(i).vertex(l).x = vertices(i).vertex(l).x*transform.scale.x + transform.position.x
			vertices(i).vertex(l).y = vertices(i).vertex(l).y*transform.scale.y + transform.position.y
			vertices(i).vertex(l).z = vertices(i).vertex(l).z*transform.scale.z + transform.position.z
		}
	}
}

class pyramid (asset : Asset) extends Mesh () {
	var vertices = new Array[Vertex](4)
	//pyramid
		vertices(0) = new Vertex( new Vector3(0,1,0), new Vector3(0,-1,-1), new Vector3(1,-1,1))
		vertices(1) = new Vertex( new Vector3(0,1,0), new Vector3(0,-1,-1), new Vector3(-1,-1,1))
		vertices(2) = new Vertex( new Vector3(0,1,0), new Vector3(1,-1,1), new Vector3(-1,-1,1))
		vertices(3) = new Vertex( new Vector3(0,-1,-1), new Vector3(1,-1,1), new Vector3(-1,-1,1))

	override def Draw () : Unit = {
		val transform = asset.transform
		for (i <- 0 to vertices.length-1; l <- 0 to vertices(i).vertex.length-1) {
			vertices(i).vertex(l).x = vertices(i).vertex(l).x*transform.scale.x + transform.position.x
			vertices(i).vertex(l).y = vertices(i).vertex(l).y*transform.scale.y + transform.position.y
			vertices(i).vertex(l).z = vertices(i).vertex(l).z*transform.scale.z + transform.position.z
		}
	}
}

class appleShape (asset : Asset) extends Mesh () {
	var vertices = new Array[Vertex](0)
	var vertices2 = new Array[Triangle](10)
	//voorkant
		vertices2(0) = new Triangle( new Vector3(0,1,0), new Vector3(0,0,1), new Vector3(-1,0,0), new Color(255, 124, 122)) //linksboven
		vertices2(1) = new Triangle( new Vector3(0,1,0), new Vector3(0,0,1), new Vector3(1,0,0), new Color(227, 27, 16)) //rechtsboven
		vertices2(2) = new Triangle( new Vector3(0,-1,0), new Vector3(0,0,1), new Vector3(-1,0,0), new Color(227, 27, 16)) //linksonder
		vertices2(3) = new Triangle( new Vector3(0,-1,0), new Vector3(0,0,1), new Vector3(1,0,0), new Color(177, 0, 0)) //rechtsonder
	//achterkant
		vertices2(4) = new Triangle( new Vector3(0,1,0), new Vector3(0,0,-1), new Vector3(-1,0,0), new Color(227, 27, 16)) //linksboven
		vertices2(5) = new Triangle( new Vector3(0,1,0), new Vector3(0,0,-1), new Vector3(1,0,0), new Color(177, 0, 0)) //rechtsboven
		vertices2(6) = new Triangle( new Vector3(0,-1,0), new Vector3(0,0,-1), new Vector3(-1,0,0), new Color(177, 0 ,0)) //linksonder
		vertices2(7) = new Triangle( new Vector3(0,-1,0), new Vector3(0,0,-1), new Vector3(1,0,0), new Color(104, 0, 0)) //rechtsonder
	//blaadje
		vertices2(8) = new Triangle( new Vector3(0,0,0), new Vector3(0.6f,0.7f,0.3f), new Vector3(0.6f,0.7f,-0.3f), new Color(16, 204, 0))
		vertices2(9) = new Triangle( new Vector3(0.8f,0.3f,0), new Vector3(0.6f,0.7f,0.3f), new Vector3(0.6f,0.7f,-0.3f), new Color(11, 113, 0))

	override def Draw () : Unit = {
		val transform = asset.transform
		for (i <- 0 to vertices2.length-1; l <- 0 to vertices2(i).vertex.length-1) {
			vertices2(i).vertex(l).x = vertices2(i).vertex(l).x*transform.scale.x + transform.position.x
			vertices2(i).vertex(l).y = vertices2(i).vertex(l).y*transform.scale.y + transform.position.y
			vertices2(i).vertex(l).z = vertices2(i).vertex(l).z*transform.scale.z + transform.position.z
			vertices2(i).Update()
			vertices2(i).dist = new Global().Distance(vertices2(i).position, new Vector3(0,0,0) )
		}
	}
}

class cubeFillShape (asset : Asset) extends Mesh () {
	var vertices = new Array[Vertex](0)
	var vertices2 = new Array[Triangle](12)
	//front
		vertices2(0) = new Triangle( new Vector3(-1,1,-1), new Vector3(1,1,-1), new Vector3(1,-1,-1), new Color(180,180,180))
		vertices2(1) = new Triangle( new Vector3(-1,1,-1), new Vector3(-1,-1,-1), new Vector3(1,-1,-1), new Color(180,180,180))
		//back
		vertices2(2) = new Triangle( new Vector3(-1,1,1), new Vector3(1,1,1), new Vector3(1,-1,1), new Color(110,110,110))
		vertices2(3) = new Triangle( new Vector3(-1,1,1), new Vector3(-1,-1,1), new Vector3(1,-1,1), new Color(110,110,110))
		//top
		vertices2(4) = new Triangle( new Vector3(-1,1,-1), new Vector3(1,1,-1), new Vector3(1,1,1), new Color(240,240,240))
		vertices2(5) = new Triangle( new Vector3(-1,1,-1), new Vector3(-1,1,1), new Vector3(1,1,1), new Color(240,240,240))
		//bottom
		vertices2(6) = new Triangle( new Vector3(-1,-1,-1), new Vector3(1,-1,-1), new Vector3(1,-1,1), new Color(80,80,80))
		vertices2(7) = new Triangle( new Vector3(-1,-1,-1), new Vector3(-1,-1,1), new Vector3(1,-1,1), new Color(80,80,80))
		//left
		vertices2(8) = new Triangle( new Vector3(-1,1,-1), new Vector3(-1,1,1), new Vector3(-1,-1,-1), new Color(140,140,140))
		vertices2(9) = new Triangle( new Vector3(-1,1,-1), new Vector3(-1,-1,1), new Vector3(-1,-1,-1), new Color(140,140,140))
		//right
		vertices2(10) = new Triangle( new Vector3(1,1,-1), new Vector3(1,1,1), new Vector3(1,-1,-1), new Color(140,140,140))
		vertices2(11) = new Triangle( new Vector3(1,1,-1), new Vector3(1,-1,1), new Vector3(1,-1,-1), new Color(140,140,140))

	override def Draw () : Unit = {
		val transform = asset.transform
		for (i <- 0 to vertices2.length-1; l <- 0 to vertices2(i).vertex.length-1) {
			vertices2(i).vertex(l).x = vertices2(i).vertex(l).x*transform.scale.x + transform.position.x
			vertices2(i).vertex(l).y = vertices2(i).vertex(l).y*transform.scale.y + transform.position.y
			vertices2(i).vertex(l).z = vertices2(i).vertex(l).z*transform.scale.z + transform.position.z
			vertices2(i).Update()
			vertices2(i).dist = new Global().Distance(vertices2(i).position, new Vector3(0,0,0) )
		}
	}
}

class pyrFillShape (asset : Asset) extends Mesh () {
	var vertices = new Array[Vertex](0)
	var vertices2 = new Array[Triangle](4)
	//pyramid
		vertices2(0) = new Triangle( new Vector3(0,1,0), new Vector3(0,-1,-1), new Vector3(1,-1,1), Color.green)
		vertices2(1) = new Triangle( new Vector3(0,1,0), new Vector3(0,-1,-1), new Vector3(-1,-1,1), Color.red)
		vertices2(2) = new Triangle( new Vector3(0,1,0), new Vector3(1,-1,1), new Vector3(-1,-1,1), Color.yellow)
		vertices2(3) = new Triangle( new Vector3(0,-1,-1), new Vector3(1,-1,1), new Vector3(-1,-1,1), Color.blue)

	override def Draw () : Unit = {
		val transform = asset.transform
		for (i <- 0 to vertices2.length-1; l <- 0 to vertices2(i).vertex.length-1) {
			vertices2(i).vertex(l).x = vertices2(i).vertex(l).x*transform.scale.x + transform.position.x
			vertices2(i).vertex(l).y = vertices2(i).vertex(l).y*transform.scale.y + transform.position.y
			vertices2(i).vertex(l).z = vertices2(i).vertex(l).z*transform.scale.z + transform.position.z
			vertices2(i).Update()
			vertices2(i).dist = new Global().Distance(vertices2(i).position, new Vector3(0,0,0) )
		}
	}
}

//-----------------------WINDOW SPECIFIC STUFF----------------------------//
//-----------------------WINDOW SPECIFIC STUFF----------------------------//
//-----------------------WINDOW SPECIFIC STUFF----------------------------//

//window
class sFrame (h : Hierarchy) extends MainFrame {
	override def closeOperation () : Unit = {
		h.Quit()
	}
}

//rendering canvas
class Canvas(pixelData : Array[Array[Color]], scr : Dimension, h : Hierarchy) extends Component {
	preferredSize = scr
  val row = pixelData.length-1		//hoeveel pixels breed
  val col = pixelData(0).length-1	//hoeveel pixels hoog
  var triangles = new ListBuffer[Triangle]() //de polygonen die we gaan renderen

  override def paintComponent(g : Graphics2D) {
    val d = size
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, 
		       java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g.setColor(Color.black);
    g.fillRect(0,0, d.width, d.height);
    for (i <- 0 to row; l <- 0 to col) {
		g.setColor(pixelData(i)(l))
		g.fill(new Rectangle2D.Double(i*d.width/row , l*d.height/col , d.width/row + 1, d.height/col + 1))
	}

	g.setColor(Color.black)
    g.fillRect(0,0, h.frame.size.width, h.frame.size.height)

    val assts = h.assets.toList.sortBy(_.transform.position.z).toArray
    for (i <- assts.length-1 to 0 by -1) {
		if (assts(i).active) {
			g.setColor(Color.green)
			assts(i).renderer.Draw(g)
		}
	}

	if (triangles.length > 0) { 
		//val trngls = triangles.toList.sortBy( _.dist ).toArray
		val trngls = triangles.toList.sortWith( _.dist > _.dist ).toArray
		for (i <- 0 to trngls.length-1) {
			//trngls(i).Update()
			g.setColor(trngls(i).color)
			val xy1 = h.cam.WorldToScreen(trngls(i).vertex(0))
			val xy2 = h.cam.WorldToScreen(trngls(i).vertex(1))
			val xy3 = h.cam.WorldToScreen(trngls(i).vertex(2))
			g.fill(new Polygon(Array(xy1.x, xy2.x, xy3.x), Array(xy1.y, xy2.y, xy3.y), 3))

			//g.setStroke(new BasicStroke(1))
			//	g.setColor(Color.black)
			//	g.drawLine(xy1.x, xy1.y, xy2.x, xy2.y)
		}
	}

	h.PostDraw(g)
  }
}


//-----------------------APP SPECIFIC STUFF----------------------------//
//-----------------------APP SPECIFIC STUFF----------------------------//
//-----------------------APP SPECIFIC STUFF----------------------------//
class Hierarchy () {
//-----------------------VARS----------------------------//
//-----------------------VARS----------------------------//
//-----------------------VARS----------------------------//
	var cam = new Camera(this)
	var worldTrans = new Transform(0, null, Vector3(0,0,0), Vector3(0,0,0), Vector3(1,1,1), null, null)
	//MousePos on screen
	var MousePos = new Point(0,0)

	//original screen res
	var screenX = 1200
	var screenY = 400
	val aspct = screenY/screenX

	//pixel res
	var pScreenX = 300
	var pScreenY = 100

	//pixels op het scherm
	val pixels = Array.ofDim[Color](pScreenX,pScreenY)

	//begin de app met testen of alle pixels het doen
	for (i <- 0 to pixels.length-1; l <- 0 to pixels(0).length-1) {
		var glbl = new Global()
		pixels(i)(l) = new Color(glbl.RandRange(1,255), glbl.RandRange(1,255), glbl.RandRange(1,255))
	}

	//maak een renderer voor de pixels, pas PostDraw() aan om direct graphics te schrijven
	val canvas = new Canvas(pixels, new Dimension(screenX,screenY), this)

	//de window
	var frame = new sFrame(this) {
		//ignoreRepaint = true
		title = "application"
		preferredSize = new Dimension(screenX,screenY)
		resizable = true
		background = Color.black
		centerOnScreen
		contents = canvas
	}

	var time = 0	//telt hoeveel frames de app runt
	var run = true	//als dit false is stopt de app
	//alle assets in de app
	var assets : List[Asset] = List(
		new Asset("origin", "Default", true, this),
		new Asset("origin2", "Default", true, this),
		new Asset("origin3", "Default", true, this)
	)

	//wordt geroepen als de app begint
	def Start () : Unit = {
		cam.FoV = 230
		assets(0).transform.position.z = 100
		assets(0).transform.scale.x = 5
		assets(0).transform.scale.y = 5
		assets(0).transform.scale.z = 5
		assets(1).transform.position.z = 500
		assets(1).transform.scale.x = 50
		assets(1).transform.scale.y = 50
		assets(1).transform.scale.z = 50
		assets(2).transform.position.z = 200
		assets(2).transform.scale.x = 25
		assets(2).transform.scale.y = 25
		assets(2).transform.scale.z = 25

		assets(1).transform.setParent(assets(0).transform)
		assets(2).transform.setParent(assets(0).transform)
		//assets(0).renderer = new rectRend(assets(0))
		//assets(0).transform.position.x = 10
		//assets(0).transform.position.y = 10
		assets(0).renderer = new pyrFillRend(assets(0))
		assets(1).renderer = new cubeFillRend(assets(1))
		assets(2).renderer = new appleRend(assets(2))
	}

	var area = 10
	//wordt geroepen elk frame
	def Update () : Unit = {
		for (i <- 0 to pixels.length-1; l <- 0 to pixels(0).length-1) {
			if (i < MousePosP.x+10 && i > MousePosP.x-10 && l < MousePosP.y+10 && l > MousePosP.y-10) {
			var glbl = new Global()
			pixels(i)(l) = new Color(glbl.RandRange(1,255), glbl.RandRange(1,255), glbl.RandRange(1,255))
			}
		}
		val sx = frame.size.width/2
		val sy = frame.size.height/2
		assets(0).transform.setPosition( new Vector3( MousePosC.x, MousePosC.y, assets(0).transform.position.z ))
		//assets(0).transform.position.x = MousePosC.x
		//assets(0).transform.position.y = MousePosC.y
		//assets(1).transform.position.x = MousePosC.x
		//assets(1).transform.position.y = MousePosC.y
		//assets(2).transform.position.x = MousePosC.x
		//assets(2).transform.position.y = MousePosC.y
		Draw()
	}
	//render shit die niet op de pixel grid hoeft te komen
	def PostDraw (g : Graphics2D) : Unit = {
		/*g.setColor(Color.black)
    		g.fillRect(0,0, frame.size.width, frame.size.height)
			g.setColor(Color.green)
			g.drawLine(0, 0, MousePos.x.toInt, MousePos.y.toInt)
			g.drawLine(frame.size.width, 0, MousePos.x.toInt, MousePos.y.toInt)
			g.drawLine(0, frame.size.height-25, MousePos.x.toInt, MousePos.y.toInt)
			g.drawLine(frame.size.width, frame.size.height-25, MousePos.x.toInt, MousePos.y.toInt)*/
		rendCurs(g)
	}

	def Draw () : Unit = {
		frame.repaint()	//render graphics
	}

	//beëindig de app
	def Quit () : Unit = {
		run = false
	}

	//voer dit uit als de app stopt
	def OnQuit () : Unit = {

	}

	//mouse position on screen in pixel grid
	def MousePosP () : Point = {
		val pixelGapX : Float = frame.size.width/pScreenX
		val pixelGapY : Float = frame.size.height/pScreenY
		val p = new Point( (MousePos.x/pixelGapX).toInt , (MousePos.y/pixelGapY).toInt)
		return p
	}

	//mouse position on screen from center, left & top are -, right active& bottom are +
	def MousePosC () : Point = {
		val sx = frame.size.width/2
		val sy = frame.size.height/2
		val p = new Point( MousePos.x+10 -sx , MousePos.y+10 -sy)
		return p
	}

	//render een cirkel op de cursor positie
	def rendCurs (g : Graphics2D) : Unit = {
		/*g.setFont(new Font("TimesRoman", Font.PLAIN, 30))
		g.setColor(Color.black)
		g.drawString("MouseX : " + MousePos.x.toInt.toString + " , MouseY : " + MousePos.y.toInt.toString, 20, 50)
		g.drawString("pMouseX : " + MousePosP.x.toInt.toString + " , pMouseY : " + MousePosP.y.toInt.toString, 20, 100)
		g.drawString("width : " + frame.size.width.toString + " , height : " + frame.size.height.toString, 20, 150)
		g.drawString("pixelX : " + pScreenX.toString + " , pixelY : " + pScreenY.toString, 20, 200)
		g.drawString("pixelGapX : " + (frame.size.width/pScreenX).toString + " , pixelGapY : " + (frame.size.height/pScreenY).toString, 20, 250)
		*/
		g.setColor(Color.red)
		g.fill(new Ellipse2D.Double(MousePos.x+13, MousePos.y+13, 7, 7))

		g.setColor(Color.white)
		g.fill(new Ellipse2D.Double(cam.WorldToScreen(new Vector3(0,0,0)).x, cam.WorldToScreen(new Vector3(0,0,0)).y, 5, 5))
	}

}