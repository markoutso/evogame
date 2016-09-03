package evogame.evolution.types

import evogame.evolution.{DNA, Organism}

class Conway(val state: Grid[Boolean], val dna: DNA[Grid[Boolean]]) extends Organism[Grid[Boolean]] {
  def alive = true
  def grow: Conway = new Conway(
    Grid(state.size, Grid.range(state.size).map { case (x, y) =>
      val nb = state.neighbors(x, y)
      val living = nb.map { case (x, y) => state.get(x, y) }.count(_ == true)
      if (state.get(x, y)) {
        if (living >= 4 || living <= 1) false else true
      } else {
        if (living == 3) true else false
      }
    }), dna)

  def generations: Stream[Grid[Boolean]] = state #:: grow.generations

}
