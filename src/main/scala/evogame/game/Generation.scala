package evogame.game

class Generation(val grid: Grid) {
  val living = grid.cells.count(_.alive)

  def evolve: Generation =
    new Generation(
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

  def diff(other: Generation) = grid.diff(other.grid)

  def iterate(times: Int): List[Generation] = {
    def recur(times: Int, g: Generation, acc: List[Generation]): List[Generation] =
      if (times <= 0) acc
      else recur(times - 1, g.evolve, g :: acc)

    recur(times, this, Nil)
  }

}