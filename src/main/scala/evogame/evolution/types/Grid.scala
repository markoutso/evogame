package evogame.evolution.types

import evogame.evolution.types.Grid.Setter
import evogame.evolution.{Gene, Organism}

import scala.math.{max, min}

object Grid {
  type Setter[A] = ((Int, Int) => A)

  def range(size: Int): IndexedSeq[(Int, Int)] = for {
    y <- 0 until size
    x <- 0 until size
  } yield (x, y)

  def create[A](size: Int, f: ((Int, Int)) => A): Grid[A] =
    new Grid(size, range(size).map(f))

  def const[A](size: Int, value: A) = new Grid(size, range(size).map(_ => value))
}


case class Grid[A](size: Int, cells: IndexedSeq[A]) {

  def get(x: Int, y: Int): A = cells(y * size + x)

  def centerCells: Seq[(Int, Int)] = Seq(
    ((size / 2) - 1, (size / 2) - 1),
    ((size / 2) - 1, size / 2),
    (size / 2, (size / 2) - 1),
    (size / 2, size / 2))

  def middle: Double = size / 2.0 - 1

  def inSame(x: Int, xp: Int) = {
    val m = middle
    (x > m && xp > m) || (x < m && xp < m)
  }

  def diff(other: Grid[A]) = cells.zip(other.cells).count { case (c1, c2) => c1 != c2 }

  def neighbors(pos: (Int, Int)): Seq[(Int, Int)] = {
    val (x, y) = pos
    for {
      xp <- (x - 1) to (x + 1)
      yp <- (y - 1) to (y + 1)
      if inBounds(xp, yp) && (xp != x || yp != y)
    } yield (xp, yp)
  }

  def inBounds(x: Int, y: Int): Boolean =
    x >= 0 && x < size && y >= 0 && y < size
}


case class Scatter[A](value: Double, empty: Setter[A]) extends Gene[Grid[A]] {
  def transform(org: Organism[Grid[A]]): Organism[Grid[A]] = {
    val coords = Grid.range(org.state.size)
    val intVal = value.toInt
    val m = org.state.middle
    val grid = Grid(org.state.size, coords.map { case (x, y) =>
      val xp = if (x > m) x - intVal else x + intVal
      val yp = if (y > m) y - intVal else y + intVal
      if (org.state.inSame(x, xp) && org.state.inSame(y, yp) && org.state.inBounds(xp, yp)) org.state.get(xp, yp) else empty(x, y)
    })
    org.fromState(grid)
  }
  def mutate(diff: Double): Gene[Grid[A]] = copy(value = value + diff)
}

case class Symmetry[A](value: Double) extends Gene[Grid[A]] {
  def transform(org: Organism[Grid[A]]): Organism[Grid[A]] = {
    if (value < 0.5) org
    else {
      val offset = org.state.size - 1
      val m = org.state.middle
      val coords = for ((x, y) <- Grid.range(org.state.size)) yield {
        if (x < m) (x, y) else (Math.abs(x - offset), y)
      }
      val grid = Grid(org.state.size, coords.map { case (x, y) => org.state.get(x, y) })
      org.fromState(grid)
    }
  }
  def mutate(diff: Double): Gene[Grid[A]] = copy(value = value + diff)
}


case class Ratio[A](value: Double, full: Setter[A], empty: Setter[A]) extends Gene[Grid[A]] {
  def transform(org: Organism[Grid[A]]): Organism[Grid[A]] =
    org.fromState(Grid(org.state.size, Grid.range(org.state.size).map { case (x, y) =>
      if (Math.random() > 1 - value) full(x, y) else empty(x, y) }))
  def mutate(diff: Double): Gene[Grid[A]] = copy(value = value + diff)
}



