package evogame.game

import math.{hypot, abs}

case class Cell(x: Int, y: Int, alive: Boolean) {
  def dist(other: Cell): Double = hypot(abs(x - other.x), abs(y - other.y))
  def coords = (x, y)
}
