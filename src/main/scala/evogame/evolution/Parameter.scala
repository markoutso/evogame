package evogame.evolution

import scala.util.Random

object Parameter {
  def random: Parameter = new Parameter(Random.nextInt(19) - 9)
}

case class Parameter(value: Int) {
  require(inBounds(value))
  def inBounds(v: Int): Boolean = v >= -9 && v <= 9
  def add(diff: Int): Parameter = if (inBounds(value + diff)) Parameter(value + diff) else this
  override def toString = value.toString
}

