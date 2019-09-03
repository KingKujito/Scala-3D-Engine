package models

import java.awt.{Color, Graphics2D, Point}
import java.awt.geom.Ellipse2D
import models.Renderer._
import scala.swing.Dimension

class Hierarchy {
  var cam = new Camera(this)
  var worldTrans = new Transform(0, None, Vector3(0,0,0), Vector3(0,0,0), Vector3(1,1,1), Map.empty, None)
  //MousePos on screen
  var MousePos = new Point(0,0)

  //original screen res
  var screenX = 1200
  var screenY = 400
  val aspct: Int = screenY/screenX

  //pixel res
  var pScreenX = 300
  var pScreenY = 100

  //pixels op het scherm
  val pixels: Array[Array[Color]] = Array.ofDim[Color](pScreenX,pScreenY)

  //begin de app met testen of alle pixels het doen
  for (i <- pixels.indices; l <- pixels(0).indices) {
    pixels(i)(l) = new Color(Global.RandRange(1,255), Global.RandRange(1,255), Global.RandRange(1,255))
  }

  //maak een renderer voor de pixels, pas PostDraw() aan om direct graphics te schrijven
  val canvas = new Canvas(pixels, new Dimension(screenX,screenY), this)

  //de window
  var frame: SFrame = new SFrame(this) {
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
    new Asset("pyramid", "Default", true, this),
    new Asset("cubeBro", "Default", true, this),
    new Asset("anApple", "Default", true, this)
  )

  //wordt geroepen als de app begint
  def Start () : Unit = {
    cam.FoV = 230
    assets(0).transform.setPositionZ(100f)
    assets(0).transform.scale.x = 5f
    assets(0).transform.scale.y = 5f
    assets(0).transform.scale.z = 5f
    assets(1).transform.setPositionZ(500f)
    assets(1).transform.scale.x = 50f
    assets(1).transform.scale.y = 50f
    assets(1).transform.scale.z = 50f
    assets(2).transform.setPositionZ(200f)
    assets(2).transform.scale.x = 25f
    assets(2).transform.scale.y = 25f
    assets(2).transform.scale.z = 25f

    assets(1).transform.setParent(Some(assets(0).transform))
    assets(2).transform.setParent(Some(assets(1).transform))
    //assets(0).renderer = new rectRend(assets(0))
    //assets(0).transform.getPosition.x = 10
    //assets(0).transform.getPosition.y = 10
    assets(0).renderer = new PyrFillRend(assets(0))
    assets(1).renderer = new CubeFillRend(assets(1))
    assets(2).renderer = new AppleRend(assets(2))

    println(assets(0).transform.children)
  }

  var area = 10
  //wordt geroepen elk frame
  def Update () : Unit = {
    for (i <- pixels.indices; l <- pixels.head.indices) {
      if (i < MousePosP.x+10 && i > MousePosP.x-10 && l < MousePosP.y+10 && l > MousePosP.y-10) {
        pixels(i)(l) = new Color(Global.RandRange(1,255), Global.RandRange(1,255), Global.RandRange(1,255))
      }
    }
    //val sx = frame.size.width/2
    //val sy = frame.size.height/2
    //assets(0).transform.setPosition(Vector3( MousePosC.x, MousePosC.y, assets(0).transform.getPosition.z ))
    //assets(0).transform.translate(Vector3(1, 0, 0)) //works
    //assets(0).transform.setPosition(Vector3(1, 0, 220)) //works
    assets(0).transform.setPosition(Vector3( MousePosC.x, MousePosC.y, 100))
    //assets(0).transform.getPosition.x = MousePosC.x
    //assets(0).transform.getPosition.y = MousePosC.y
    //assets(1).transform.getPosition.x = MousePosC.x
    //assets(1).transform.getPosition.y = MousePosC.y
    //assets(2).transform.getPosition.x = MousePosC.x
    //assets(2).transform.getPosition.y = MousePosC.y
    assets(1).transform.rotate(Vector3(0.1f, 1f, 0))
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

  //mouse getPosition on screen in pixel grid
  def MousePosP : Point = {
    val pixelGapX : Float = frame.size.width/pScreenX
    val pixelGapY : Float = frame.size.height/pScreenY
    new Point( (MousePos.x/pixelGapX).toInt , (MousePos.y/pixelGapY).toInt)
  }

  //mouse getPosition on screen from center, left & top are -, right active& bottom are +
  def MousePosC : Point = {
    val sx = frame.size.width/2
    val sy = frame.size.height/2
    new Point( MousePos.x+10 -sx , MousePos.y+10 -sy)
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
    g.fill(new Ellipse2D.Double(cam.WorldToScreen(Vector3(0,0,0)).x, cam.WorldToScreen(Vector3(0,0,0)).y, 5, 5))
  }

}
