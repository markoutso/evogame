package evogame.game

class Organism(val grid: Grid) {
  val population = grid.cells.count(_.alive)

  def grow: Organism =
    new Organism(
      new Grid(grid.width, grid.height, grid.cells.map { c =>
        val nb = grid.neighbors(c).count(_.alive)
        if (c.alive) {
          if (nb >= 4 || nb <= 1) Cell(c.x, c.y, alive = false)
          else c
        } else {
          if (nb == 3) Cell(c.x, c.y, alive = true)
          else c
        }
      }))

  def diff(other: Organism) = grid.diff(other.grid)

  def iterate: Stream[Organism] = this #:: this.grow.iterate

  def advance(n: Int): Organism = {
    var current = this
    for (i <- 0 until n) current = current.grow
    current
  }

  override def toString = s"Organism(${grid.cells.filter(_.alive).toString})"

}