package models

class Asset (name_ : String, tag_ : String, active_ : Boolean, h_ : Hierarchy) {
  var h: Hierarchy = h_
  var name: String = name_
  var tag: String = tag_
  var active: Boolean = active_
  var renderer : Renderer = new Renderer(this)
  val id: Int = new Global().RandRange(0,999999)
  var transform = new Transform(id, None, Vector3(0,0,0), Vector3(0,0,0), Vector3(0,0,0), Map.empty, Some(this))
}
