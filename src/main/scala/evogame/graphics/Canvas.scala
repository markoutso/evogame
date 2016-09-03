package evogame.graphics

import evogame.evolution.types.Grid
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js
import scala.scalajs.js.timers.SetIntervalHandle

class Canvas(id: String) {
  val canvas = dom.document.getElementById(id).asInstanceOf[html.Canvas]
  val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  ctx.strokeStyle = "#d3d3d3"
  ctx.lineWidth = 1

  def withFillStyle(alive: Boolean): Canvas = {
    if (alive) ctx.fillStyle = "black" else ctx.fillStyle = "white"
    this
  }

  def drawPixel(x: Int, y: Int, scaleX: Int, scaleY: Int): Canvas = {
    ctx.beginPath()
    ctx.rect(x * scaleX, y * scaleY, scaleX, scaleY)
    ctx.fill()
    ctx.stroke()
    this
  }

  def draw(g: Grid[Boolean]): Canvas = {
    val scaleX = canvas.width / g.size
    val scaleY = canvas.height / g.size
    Grid.range(g.size).zip(g.cells).map { case ((x, y), alive) => withFillStyle(alive).drawPixel(x, y, scaleX, scaleY) }
    this
  }

  type Stop = () => Unit

  def animate(s: Stream[Grid[Boolean]], speed: Double = 200, frames: Int = -1): Stop = {
    var current = s
    var times = 0

    def clear(handle: SetIntervalHandle) = () => js.timers.clearInterval(handle)

    lazy val handle: SetIntervalHandle = js.timers.setInterval(speed) {
      if (frames > 0 && times > frames - 1) clear(handle)()
      else {
        draw(current.head)
        times += 1
        current = current.tail
      }
    }
    clear(handle)
  }

}