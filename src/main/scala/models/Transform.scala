package models

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
