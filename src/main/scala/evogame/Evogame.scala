package evogame


import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js.JSApp

object Conway {

  case class Cell(x: Int, y: Int, alive: Boolean)

  class Conway(rows: Int, cols: Int, state: Set[(Int, Int)]) {

    val model: IndexedSeq[Cell] = for (i <- 0 until rows; j <- 0 until cols) yield {
      if (state.contains((i, j))) Cell(i, j, true)
      else Cell(i, j, false)
    }

    def get(i: Int, j: Int): Cell = model(i * cols + j)

    def neighbors(c: Cell): Seq[Cell] =
      for {
        i <- c.x - 1 to c.x + 1
        j <- c.y - 1 to c.y + 1
        if inBounds(i, j) && (i != c.x || j != c.y)
      } yield get(i, j)

    def inBounds(i: Int, j: Int): Boolean = i > 0 && i < rows && j > 0 && j < cols

  }


}


object Evogame extends JSApp {
  def main() = {
    val canvas = dom.document.getElementById("canvas").asInstanceOf[html.Canvas]
    val renderer = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.width = canvas.parentElement.clientWidth
    canvas.height = canvas.parentElement.clientHeight

    renderer.fillStyle = "black"

    var down = false
    canvas.onmousedown =
      (e: dom.MouseEvent) => down = true

    canvas.onmouseup =
      (e: dom.MouseEvent) => down = false

    canvas.onmousemove = {
      (e: dom.MouseEvent) =>
        val rect =
          canvas.getBoundingClientRect()
        if (down) renderer.fillRect(
          e.clientX - rect.left,
          e.clientY - rect.top,
          10, 10
        )
    }
  }


}
