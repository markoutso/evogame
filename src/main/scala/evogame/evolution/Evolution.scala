package evogame.evolution


  trait Organism[A, B <: Organism[A, B]] {
    def state: A
    def dna: DNA[A]
    def alive: Boolean
    def withState(state: A): B
    def copy: B
  }



  trait DNA[A] {
    def genes: IndexedSeq[Gene[A]]
    def mutate: DNA[A]
  }

  trait Gene[A] {
    def transform[B <: Organism[A, B]](org: B): B
    def value: Double
    def mutate: Gene[A]
  }


//
//trait Species[A] {
//  def organism: Organism[A]
//  def evolve: Stream[Organism[A]] = {
//    val nextDna = organism.dna
//    val nextGen = nextDna.genes.foldLeft(organism) { case (generation, gene) => gene.transform(generation) }
//    (nextGen #:: this.evolve).distinct
//  }
//
//}

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

