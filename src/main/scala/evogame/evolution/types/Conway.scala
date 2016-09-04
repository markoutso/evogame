package evogame.evolution.types

import evogame.evolution.{DNA, Organism}

case class Conway(state: Grid[Boolean], dna: DNA[Grid[Boolean]]) extends Organism[Grid[Boolean], Conway] {
  def alive = true
  def grow: Conway = {
    val cells = Grid.range(state.size).map { case (x, y) =>
      val nb = state.neighbors(x, y)
      val living = nb.map { case (_x, _y) => state.get(_x, _y) }.count(_ == true)
      if (state.get(x, y)) living == 2 || living == 3
      else living == 3
    }
    Conway(Grid(state.size, cells), dna)
  }

  def withState(state: Grid[Boolean]): Conway = Conway(state, dna)
  def copy = Conway(state, dna.mutate)
  def generations: Stream[Grid[Boolean]] = state #:: grow.generations


}
