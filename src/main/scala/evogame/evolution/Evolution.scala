package evogame.evolution

  trait Organism[A] {
    def state: A
    def grow: Organism[A]
    def dna: DNA[A]
    def alive: Boolean
    def generations: Stream[A]
    def fromState(state: A): Organism[A]
  }

  trait Species[A] {
    def organism: Organism[A]
    def evolve: Stream[Organism[A]] = {
      val nextDna = organism.dna
      val nextGen = nextDna.genes.foldLeft(organism) { case (generation, gene) => gene.transform(generation) }
      (nextGen #:: this.evolve).distinct
    }

  }

  trait Creator[A] {
    def create(from: A): DNA[A]
  }

  trait DNA[A] {
    def genes: IndexedSeq[Gene[A]]
    def mutate: DNA[A]
  }

  trait Gene[A] {
    def transform(org: Organism[A]): Organism[A]
    def value: Double
    def mutate(diff: Double): Gene[A]
  }


//  object RandomMutator extends Mutator {
//    def randomTerm: Int = if (Math.random() > 0.5) 2 else -2
//    def apply(dna: DNA): DNA =
//      if (dna.nonEmpty) {
//        val rand = Random.nextInt(dna.length)
//        val gene = dna(rand)
//        val newGene = gene.mutate(gene.param.add(randomTerm))
//        dna.updated(rand, newGene)
//      }
//      else dna
//  }


//  object OrderMutator extends Mutator {
//    def apply(dna: DNA): DNA =
//      if (dna.nonEmpty) {
//        val rand = Random.nextInt(dna.length)
//        val next = if (rand + 1 == dna.length) 0 else rand + 1
//        val tmp = dna(rand)
//        dna.updated(rand, dna(next)).updated(next, tmp)
//      }
//     else dna
//  }

