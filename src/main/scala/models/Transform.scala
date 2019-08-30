package models

class Transform (id : Int, parent_ : Option[Transform] = None, position_ : Vector3, rotation_ : Vector3, scale_  : Vector3, child_ : Map[Int, Transform] = Map.empty, asset_ : Option[Asset] = None) {
  val ID: Int = id
  var parent: Option[Transform] = parent_		//waar zit deze transform aan vast
  private var position: Vector3 = position_	//positie in de wereld
  var rotation: Vector3 = rotation_	//rotatie in de wereld
  var scale: Vector3 = scale_			//grootte in de wereld
  var child: Map[Int, Transform] = child_			//kinder transforms [id, transform]
  var asset: Option[Asset] = asset_			//asset waar de transform aan vast zit

  //geef een list met alle kinder transforms
  def children : List[Transform] = child.values.toList

  //koppel een ouder
  def setParent (t : Option[Transform]) : Unit = {
    if(t.isEmpty) {
      parent.foreach(_.child -= ID)
      parent = None
    } else {
      parent = t
      t.foreach(_.child += (ID -> this))
    }
  }

  //beweeg
  def translate(w : Vector3) : Unit = {
    this.position.x += w.x
    this.position.y += w.y
    this.position.z += w.z
    //pas ook toe op kinderen
    children.foreach { a =>
      a.translate(w)
    }
  }

  //vergroot of verklein
  def grow(w : Vector3) : Unit = {
    this.scale.x += w.x
    this.scale.y += w.y
    this.scale.z += w.z
    children.foreach { a =>
      a.grow(w)
      a.position = Mesh.applyScale(a.position, this)
    }
  }

  //draai
  def rotate(w : Vector3) : Unit = {
    this.rotation.x += w.x
    this.rotation.y += w.y
    this.rotation.z += w.z
    children.foreach { a =>
      a.rotate(w)
    }
  }

  //geef gegevens weer in locale waarde
  def localPosition : Vector3 = {
    if (parent.isDefined) {
      val w : Vector3 = Vector3( this.position.x - parent.get.position.x, this.position.y - parent.get.position.y, this.position.z - parent.get.position.z )
      w
    } else {
      this.position
    }
  }

  def localRotation : Vector3 = {
    if (parent.isDefined) {
      val w : Vector3 = Vector3( this.rotation.x - parent.get.rotation.x, this.rotation.y - parent.get.rotation.y, this.rotation.z - parent.get.rotation.z )
      w
    } else {
      this.rotation
    }
  }

  def localScale : Vector3 = {
    if (parent.isDefined) {
      val w : Vector3 = Vector3( this.scale.x - parent.get.scale.x, this.scale.y - parent.get.scale.y, this.scale.z - parent.get.scale.z )
      w
    } else {
      this.scale
    }
  }

  def setLocalPosition (v : Vector3) : Unit = {
    if(parent.isDefined) {
      val w: Vector3 = Vector3(parent.get.position.x + v.x, parent.get.position.y + v.y, parent.get.position.z + v.z)
      this.position = w
    } else {
      setPosition(v)
    }
  }

  def setLocalRotation (v : Vector3) : Unit = {
    if(parent.isDefined) {
      val w: Vector3 = Vector3(parent.get.rotation.x + v.x, parent.get.rotation.y + v.y, parent.get.rotation.z + v.z)
      this.rotation = w
    } else {
      setRotation(v)
    }
  }

  def setLocalScale (v : Vector3) : Unit = {
    if(parent.isDefined) {
      val w: Vector3 = Vector3(parent.get.localScale.x + v.x, parent.get.localScale.y + v.y, parent.get.localScale.z + v.z)
      this.scale = w
    } else {
      setScale(v)
    }
  }


  def setPosition (v : Vector3) : Unit = {
    if (children.nonEmpty) {
      val a = children.toArray
      for (i <- a.indices) {
        val v2 = Vector3 ( v.x + a(i).localPosition.x , v.y + a(i).localPosition.y , v.z + a(i).localPosition.z )
        a(i).setPosition( v2 )
      }
    }
    this.position = v
  }

  def setPositionX (f : Float) : Unit = {
    children.foreach{ a =>
      a.setPosition( Vector3 ( a.position.x + f , a.position.y , a.position.z ) )
    }
    setPosition( Vector3 ( position.x + f , position.y , position.z) )
  }

  def setPositionY (f : Float) : Unit = {
    children.foreach{ a =>
      a.setPosition( Vector3 ( a.position.x , a.position.y + f , a.position.z) )
    }
    setPosition( Vector3 ( position.x , position.y + f , position.z) )
  }

  def setPositionZ (f : Float) : Unit = {
    children.foreach{ a =>
        a.setPosition( Vector3 ( a.position.x , a.position.y , a.position.z + f ) )
    }
    setPosition( Vector3 ( position.x , position.y , position.z + f ) )
  }

  def setRotation (v : Vector3) : Unit = {
    if (children.nonEmpty) {
      val a = children.toArray
      for (i <- a.indices) {
        val v2 = Vector3 ( v.x + a(i).localRotation.x , v.y + a(i).localRotation.y , v.z + a(i).localRotation.z )
        a(i).setRotation( v2 )
      }
    }
    this.rotation = v
  }

  def setScale (v : Vector3) : Unit = {
    if (children.nonEmpty) {
      val a = children.toArray
      for (i <- a.indices) {
        val v2 = Vector3 ( a(i).localScale.x + v.x , a(i).localScale.y + v.y , a(i).localScale.z + v.z )
        a(i).setScale( v2 )
      }
    }
    this.scale = v
  }

  def getPosition: Vector3 = if(parent.isDefined) {
    //TODO get this to work properly dammit
    val v = Mesh.applyRotation(Vector3(0,0,0), parent.get)
    Vector3(parent.get.position.x + v.x, parent.get.position.y + v.y, parent.get.position.z + v.z)
    position
  } else position
}
