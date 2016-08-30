package evogame.evolution


case class Parameter(value: Int) {
  require(inBounds(value))
  def inBounds(v: Int): Boolean = v >= -9 && v <= 9
  def add(diff: Int): Parameter = if (inBounds(value + diff)) Parameter(value + diff) else this
}

