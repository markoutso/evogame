package evogame

import evogame.game.{Generation, Grid}
import evogame.graphics.Canvas
import evogame.evolution.evolution._

import scalajs.js
import scala.scalajs.js.JSApp

object Evogame extends JSApp {

  def animate(g: Generation, c: Canvas): Unit = {
    var current = g
    js.timers.setInterval(100) {
      c.draw(current)
      current = current.evolve
    }
  }

  def main() = {
    val canvas = new Canvas("canvas")
    val beacon = new Generation(
      Grid.fromCoordinates(6, 6, List((1, 1), (1, 2), (2, 1), (2, 2), (3, 3), (3, 4), (4, 3), (4, 4)))
    )
    val gun = new Generation(
      Grid.fromMatrixCoordinates(80, 80, List(
        (5, 1), (5, 2), (6, 1), (6, 2), (5, 11), (6, 11), (7, 11), (4, 12),
        (3, 13), (3, 14), (8, 12), (9, 13), (9, 14), (6, 15), (4, 16), (5, 17),
        (6, 17), (7, 17), (6, 18), (8, 16), (3, 21), (4, 21), (5, 21), (3, 22),
        (4, 22), (5, 22), (2, 23), (6, 23), (1, 25), (2, 25), (6, 25), (7, 25), (3, 35), (4, 35), (3, 36), (4, 36)))
    )

    val base = Size(20, 20)
    val games = (0 until 10).map(_ => Ratio(0.2)(base))
    val result = games.sortBy(-_.iterate(10).head.living).head

    //animate(games.head, canvas)

    val W = 20
    val H = 20
    val before = new Generation(Grid.empty(W, H))
    val after = new Generation(Grid.fromCoordinates(W, H, before.grid.centerCells.map(_.coords)))
    val later = Scatter(-8)(Scatter(8)(after))
    canvas.draw(later)


  }


}
