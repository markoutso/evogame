package evogame.evolution

import evogame.game.{Cell, Animal, Grid}
import scala.util.Random.shuffle

object evolution {

  type Generator[A] = A => Animal
  type DNA = List[Gene]
  type Mutator = DNA => DNA

  object Species {
    def apply(_initial: Animal, _dna: DNA, _mutators: List[Mutator]): Species = new Species {
      val dna = _dna
      val mutators = _mutators
      val animal = _initial
    }
  }

  trait Species {
    def animal: Animal
    def mutators: List[Mutator]
    def dna: DNA

    def evolve: Stream[Species] = {
      val nextDna = mutators.foldLeft(dna) { case (_dna, mut) => mut(_dna) }
      val nextGen = nextDna.foldLeft(animal) { case (generation, gene) => gene(generation)}
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

  trait Gene {
    def param: Parameter
    def apply(in: Animal): Animal
    def mutate(p: Parameter): Gene
  }

  case class Scatter(param: Parameter) extends Gene {
    private val normalParam = Math.round(param.value / 2.0).toInt
    def apply(in: Animal): Animal = {
      val (mx, my) = in.grid.middle
      val living = for {
        c <- in.grid.cells if c.alive
        xd = if (c.x > mx) 1 else -1
        yd = if (c.y > my) 1 else -1
        (newX, newY) = (c.x + (xd * normalParam), c.y + (yd * normalParam))
        if in.grid.inBounds(newX, newY)
      } yield (newX, newY)
      new Animal(Grid.fromCoordinates(in.grid.width, in.grid.height, living))
    }
    def mutate(p: Parameter) = copy(param = p)
  }

  case class Ratio(param: Parameter) extends Gene {
    private val normalParam = (param.value + 9) / 18.0
    def apply(in: Animal): Animal = new Animal(new Grid(in.grid.width, in.grid.height,
        Grid.rangeMap(in.grid.width, in.grid.height) {
          case (x, y) if Math.random() > 1 - normalParam => Cell(x, y, alive = true)
          case (x, y) => Cell(x, y, alive = false)
        }))

    def mutate(p: Parameter) = copy(param = p)
  }

  object Empty extends Generator[(Int, Int)] {
    def apply(params: (Int, Int)) = params match {
      case (width, height) => new Animal(Grid.empty(width, height))
    }
  }

}
