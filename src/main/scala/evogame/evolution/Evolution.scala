package evogame.evolution

import evogame.game.{Cell, Generation, Grid}

object evolution {

  trait Generator[A] extends (A => Generation)

  trait Gene extends Generator[Generation]

  case class Scatter(param: Int) extends Gene {
    def apply(in: Generation) = {
      val (mx, my) = in.grid.middle
      println(in.grid.middle)
      val living = for {
        c <- in.grid.cells if c.alive
        xd = if (c.x > mx) 1 else -1
        yd = if (c.y > my) 1 else -1
        (newX, newY) = (c.x + (xd * param), c.y + (yd * param))
        if in.grid.inBounds(newX, newY)
      } yield (newX, newY)
      new Generation(Grid.fromCoordinates(in.grid.width, in.grid.height, living))
    }
  }
  
  case class Ratio(ratio: Double) extends Gene {
    def apply(in: Generation): Generation = new Generation(
      new Grid(in.grid.width, in.grid.height,
        Grid.rangeMap(in.grid.width, in.grid.height) {
          case (x, y) if Math.random() > 1 - ratio => Cell(x, y, alive = true)
          case (x, y) => Cell(x, y, alive = false)
        }))
  }

  object Size extends Generator[(Int, Int)] {
    def apply(params: (Int, Int)) = params match {
      case (width, height) => new Generation(Grid.empty(width, height))
    }
  }

}
