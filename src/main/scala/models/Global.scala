package models

class Global () {
  //afstand in 3d ruimte
  def Distance (v1 : Vector3, v2 : Vector3) : Float =
    math.sqrt(math.pow(v2.x-v1.x , 2)+math.pow(v2.y-v1.y , 2)+math.pow(v2.z-v1.z , 2)).toFloat

  //willekeurige float tussen min en max
  def RandRange (min : Float, max : Float) : Float =
    (math.random * (max-min) + min).toFloat

  //willekeurige int tussen min en max
  def RandRange (min : Int, max : Int) : Int =
    (math.random * (max-min) + min).toInt

  //geef een list van alle assets met naam s in lijst l
  def findAssets (s : String, l : List[Asset]) : List[Asset] = for (a <- l if a.name == s) yield a

  //vind de eerste asset met naam s in lijst l
  def find (s : String, l : List[Asset]) : Asset = findAssets(s,l).head

  //geef een list van alle assets met tag s in lijst l
  def findAssetsWithTag (s : String, l : List[Asset]) : List[Asset] = for (a <- l if a.tag == s) yield a

  //vind de eerste asset met tag s in lijst l
  def findWithTag (s : String, l : List[Asset]) : Asset = findAssetsWithTag(s,l).head
}
