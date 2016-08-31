package evogame.evolution

import evogame.game.{Cell, Organism, Grid}
import scala.util.Random.shuffle
import scala.language.reflectiveCalls
import scala.language.existentials

object evolution {

  type Generator[A] = A => Organism
  type DNA = List[Gene]
  type Mutator = DNA => DNA


  trait Gene {
    def param: Parameter
    def apply(in: Organism): Organism
    def mutate(p: Parameter): Gene
    def normalize(p: Parameter): Any
  }

  type GeneCopy = {def copy(param: Parameter): Gene}

  trait CopyMutation {
    self: GeneCopy =>
    def mutate(p: Parameter): Gene = {
      copy(param = p)
    }
  }

  trait BasicGene extends Gene with CopyMutation {
    self: GeneCopy =>
  }

  case class Species(org: Organism, dna: DNA, mutators: List[Mutator]) {
    def evolve: Stream[Species] = {
      val nextDna = mutators.foldLeft(dna) { case (_dna, mut) => mut(_dna) }
      val nextGen = nextDna.foldLeft(org) { case (generation, gene) => gene(generation) }
      val newSpecies = Species(nextGen, nextDna, mutators)
      newSpecies #:: this.evolve // first generation ancestors
    }
  }

  object RandomMutator extends Mutator {
    def random: Int = if (Math.random() > 0.5) 1 else -1
    def apply(dna: DNA): DNA = shuffle(dna) match {
      case x :: xs => x.mutate(x.param.add(random)) :: xs
      case _ => Nil
    }
  }

  case class Scatter(param: Parameter) extends BasicGene {
    override def normalize(p: Parameter): Int = Math.round(param.value / 2.0).toInt
    def apply(in: Organism): Organism = {
      val normalParam = normalize(param)
      val (mx, my) = in.grid.middle
      val living = for {
        c <- in.grid.cells if c.alive
        xd = if (c.x > mx) 1 else -1
        yd = if (c.y > my) 1 else -1
        (newX, newY) = (c.x + (xd * normalParam), c.y + (yd * normalParam))
        if in.grid.inBounds(newX, newY)
      } yield (newX, newY)
      new Organism(Grid.fromCoordinates(in.grid.width, in.grid.height, living))
    }
  }

  case class Symmetry(param: Parameter) extends BasicGene {

    /*
      -1 -> keep left
      1 -> keep right
      0 -> keep both
     */
    override def normalize(p: Parameter): Int = if (p.value > 0) 1 else if (p.value < 0) -1 else 0

    def apply(in: Organism): Organism = {
      val normalParam = normalize(param)
      val middleX = in.grid.middle._1
      if (normalParam == 0) in
      else {
        val offset = in.grid.width - 1
        val coordinates =
          if (normalParam == -1) {
            val cellCoords = for (c <- in.grid.cells if c.alive  && c.x <= middleX) yield c.coords
            cellCoords ++ cellCoords.map { case (x, y) => (offset - x, y) }
          }
          else {
            val cellCoords = for (c <- in.grid.cells if c.alive  && c.x > middleX) yield c.coords
            cellCoords ++ cellCoords.map { case (x, y) => (Math.abs(x - offset), y) }
          }
        //println((grid.map(_.x).min, grid.map(_.x).max, grid.map(_.y).min, grid.map(_.y).max))
        new Organism(Grid.fromCoordinates(in.grid.width, in.grid.height, coordinates))
      }
    }
  }

  case class Ratio(param: Parameter) extends BasicGene {
    override def normalize(p: Parameter) = (param.value + 9) / 18.0
    def apply(in: Organism): Organism = {
      val normalParam = normalize(param)
      new Organism(new Grid(in.grid.width, in.grid.height,
        Grid.rangeMap(in.grid.width, in.grid.height) {
          case (x, y) if Math.random() > 1 - normalParam => Cell(x, y, alive = true)
          case (x, y) => Cell(x, y, alive = false)
        }))
    }
  }

  object Empty extends Generator[(Int, Int)] {
    def apply(params: (Int, Int)) = params match {
      case (width, height) => new Organism(Grid.empty(width, height))
    }
  }

  object Full extends Generator[(Int, Int)] {
    def apply(params: (Int, Int)) = params match {
      case (width, height) => new Organism(Grid.full(width, height))
    }
  }

}
