package evogame.game

final class Organism(val grid: Grid) {
  val population = grid.cells.count(_.alive)
  def moving = this.grid.diff(nextGrid)

  lazy val nextGrid = new Grid(grid.width, grid.height, grid.cells.map { c =>
    val nb = grid.neighbors(c).count(_.alive)
    if (c.alive) {
      if (nb >= 4 || nb <= 1) Cell(c.x, c.y, alive = false)
      else c
    } else {
      if (nb == 3) Cell(c.x, c.y, alive = true)
      else c
    }
  })

  def grow: Organism = new Organism(nextGrid)

  def iterate: Stream[Organism] = this #:: this.grow.iterate

  def advance(n: Int): Organism = {
    var current = this
    for (i <- 0 until n) current = current.grow
    current
  }
  def canEqual(a: Any) = a.isInstanceOf[this.type]

  override def equals(that: Any): Boolean = that match {
      case that: Organism => this.hashCode == that.hashCode
      case _ => false
    }
  override def hashCode: Int = this.grid.cells.hashCode

  override def toString = s"Organism(${grid.cells.filter(_.alive).map(_.coords)})"

}