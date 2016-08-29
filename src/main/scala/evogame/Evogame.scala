package evogame


import evogame.Game.{Cell, Game}
import org.scalajs.dom
import org.scalajs.dom.html
import scalajs.js

import scala.scalajs.js.JSApp

object Game {

  def rangeMap(width: Int, height: Int)(f: (Int, Int) => Cell) =
    for {
      y <- 0 until height
      x <- 0 until width
    } yield f(x, y)


  case class Cell(x: Int, y: Int, alive: Boolean)

  def fromCoordinates(width: Int, height: Int, living: Seq[(Int, Int)]): Game = {
    val lookup = living.toSet
    new Game(
      rangeMap(height, width) { case (x, y) =>
        if (lookup.contains((x, y))) Cell(x, y, alive = true)
        else Cell(x, y, alive = false)
      }
    )
  }

  def fromMatrixPositions(width: Int, height: Int, living: Seq[(Int, Int)]): Game =
    fromCoordinates(width, height, living.map(_.swap))

  class Game(val model: IndexedSeq[Cell]) {
    private def addOne(t: (Int, Int)): (Int, Int) = (t._1 + 1, t._2 + 1)

    val (width, height) = addOne(model.foldLeft((0, 0)) { case ((x_, y_), Cell(x, y, _)) =>
      (Math.max(x_, x), Math.max(y_, y))
    })

    val living = model.count(_.alive)

    println(living)


    def get(x: Int, y: Int): Cell = model(y * width + x)

    def neighbors(c: Cell): Seq[Cell] =
      for {
        x <- (c.x - 1) to (c.x + 1)
        y <- (c.y - 1) to (c.y + 1)
        if inBounds(x, y) && (x != c.x || y != c.y)
      } yield get(x, y)

    def inBounds(x: Int, y: Int): Boolean =
      x >= 0 && x < width && y >= 0 && y < height

    def evolve: Game = new Game(model.map { c =>
      val nb = neighbors(c).count(_.alive)
      if (c.alive) {
        if (nb >= 4 || nb <= 1) Cell(c.x, c.y, alive = false)
        else c
      } else {
        if (nb == 3) Cell(c.x, c.y, alive = true)
        else c
      }
    })
  }

  trait Generator[A] extends (A => Game)

  trait Gene extends Generator[Game]

  case class Ratio(ratio: Double) extends Gene {
    def apply(in: Game): Game = new Game(rangeMap(in.width, in.height) {
      case (x, y) if Math.random() > 1 - ratio => Cell(x, y, alive = true)
      case (x, y) => Cell(x, y, alive = false)
    })
  }

  val Size = new Generator[(Int, Int)] {
    def apply(params: (Int, Int)) = params match {
      case (rows, cols) => new Game(rangeMap(rows, cols) { case (i, j) => Cell(i, j, alive = false) })
    }
  }
}

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

  def drawGame(g: Game): this.type = {
    val scaleX = canvas.width / g.width
    val scaleY = canvas.height / g.height
    g.model.foreach(c => withFillStyle(c).drawPixel(c.x, c.y, scaleX, scaleY))
    this
  }

}

object Evogame extends JSApp {

  import Game._

  def animate(g: Game, c: Canvas): Unit = {
    var current = g
    js.timers.setInterval(50) {
      c.drawGame(current)
      current = current.evolve
    }
  }

  def main() = {
    val canvas = new Canvas("canvas")

    val beacon = Game.fromCoordinates(6, 6, List((1, 1), (1, 2), (2, 1), (2, 2), (3, 3), (3, 4), (4, 3), (4, 4)))

    val gun = Game.fromMatrixPositions(80, 80,
      List(
        (5, 1), (5, 2), (6, 1), (6, 2), (5, 11), (6, 11), (7, 11), (4, 12),
        (3, 13), (3, 14), (8, 12), (9, 13), (9, 14), (6, 15), (4, 16), (5, 17),
        (6, 17), (7, 17), (6, 18), (8, 16), (3, 21), (4, 21), (5, 21), (3, 22),
        (4, 22), (5, 22), (2, 23), (6, 23), (1, 25), (2, 25), (6, 25), (7, 25), (3, 35), (4, 35), (3, 36), (4, 36)))


    val random = Ratio(0.2)(Size(20, 20))

    animate(gun, canvas)


  }


}
