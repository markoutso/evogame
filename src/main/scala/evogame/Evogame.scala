package evogame

import evogame.evolution.Parameter
import evogame.evolution.evolution._
import evogame.game.{Grid, Organism}
import evogame.graphics.Canvas

import scala.scalajs.js.JSApp

object Evogame extends JSApp {

  def main() = {
    val canvas = new Canvas("canvas")
    val beacon = new Organism(
      Grid.fromCoordinates(6, 6, List((1, 1), (1, 2), (2, 1), (2, 2), (3, 3), (3, 4), (4, 3), (4, 4)))
    )
    val gun = new Organism(
      Grid.fromMatrixCoordinates(80, 80, List(
        (5, 1), (5, 2), (6, 1), (6, 2), (5, 11), (6, 11), (7, 11), (4, 12),
        (3, 13), (3, 14), (8, 12), (9, 13), (9, 14), (6, 15), (4, 16), (5, 17),
        (6, 17), (7, 17), (6, 18), (8, 16), (3, 21), (4, 21), (5, 21), (3, 22),
        (4, 22), (5, 22), (2, 23), (6, 23), (1, 25), (2, 25), (6, 25), (7, 25), (3, 35), (4, 35), (3, 36), (4, 36))))

    val W = 20
    val H = 20

    val initial = Species(Empty(W, H), List(Scatter(Parameter(2)), Ratio(Parameter(-7)), Symmetry(Parameter(-9))), List(RandomMutator))
    val best = (1 until 10).foldLeft(initial) {
      case (s, i) =>
        println(s"evolving $i species")
        s.evolve.take(10).maxBy(_.org.advance(100).moving)
    }
    canvas.animate(best.org.iterate.map(_.grid), 500)

  }


}
