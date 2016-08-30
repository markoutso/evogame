package evogame.game

object Grid {

  def rangeMap[A](width: Int, height: Int)(f: (Int, Int) => A) =
    for {
      y <- 0 until height
      x <- 0 until width
    } yield f(x, y)

  def fromCoordinates(width: Int, height: Int, living: Seq[(Int, Int)]): Grid = {
    val lookup = living.toSet
    new Grid(width, height, rangeMap(height, width) { case (x, y) =>
      if (lookup.contains((x, y))) Cell(x, y, alive = true)
      else Cell(x, y, alive = false)
    })
  }

  def fromMatrixCoordinates(width: Int, height: Int, living: Seq[(Int, Int)]): Grid =
    fromCoordinates(width, height, living.map(_.swap))

  def empty(width: Int, height: Int): Grid = new Grid(
    width, height, rangeMap(width, height)((x, y) => Cell(x, y, alive=false))
  )

}

class Grid(val width: Int, val height: Int, val cells: IndexedSeq[Cell]) {
  type Dir = (Int, Int)

  def get(x: Int, y: Int): Cell = cells(y * width + x)

  def centerCells: Seq[Cell] = {
    val dirs = Seq(
      ((width / 2) - 1, (height / 2) - 1),
      ((width / 2) - 1, height / 2),
      (width / 2, (height / 2) - 1),
      (width / 2, height / 2))
    dirs.map { case (x, y) => get(x, y) }
  }

  def middle: (Double, Double) = (width / 2.0 - 1, height / 2.0 - 1)

  def outerCells: Seq[Cell] = {
    val (mw, mh) = middle
    for {
      x <- List(0, mw.toInt, width - 1)
      y <- List(0, mh.toInt, height - 1)
      if (x, y) != (mw, mh)
    } yield get(x, y)
  }


  def diff(other: Grid) = cells.zip(other.cells).count { case (c1, c2) => c1.alive != c2.alive }

  def neighbors(c: Cell): Seq[Cell] =
    for {
      x <- (c.x - 1) to (c.x + 1)
      y <- (c.y - 1) to (c.y + 1)
      if inBounds(x, y) && (x != c.x || y != c.y)
    } yield get(x, y)

  def inBounds(x: Int, y: Int): Boolean =
    x >= 0 && x < width && y >= 0 && y < height

}
