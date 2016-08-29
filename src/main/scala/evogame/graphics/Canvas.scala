package evogame.graphics

import evogame.game.{Cell, Generation}
import org.scalajs.dom
import org.scalajs.dom.html

class Canvas(id: String) {
  val canvas = dom.document.getElementById(id).asInstanceOf[html.Canvas]
  val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  ctx.strokeStyle = "#d3d3d3"
  ctx.lineWidth = 1

  def withFillStyle(c: Cell): Canvas = {
    if (c.alive) ctx.fillStyle = "black" else ctx.fillStyle = "white"
    this
  }

  def drawPixel(x: Int, y: Int, scaleX: Int, scaleY: Int): this.type = {
    ctx.beginPath()
    ctx.rect(x * scaleX, y * scaleY, scaleX, scaleY)
    ctx.fill()
    ctx.stroke()
    this
  }

  def draw(g: Generation): this.type = {
    val scaleX = canvas.width / g.grid.width
    val scaleY = canvas.height / g.grid.height
    g.grid.cells.map(c => withFillStyle(c).drawPixel(c.x, c.y, scaleX, scaleY))
    this
  }

}