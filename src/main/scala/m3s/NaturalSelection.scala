package m3s

trait Species[A] {
  //TODO: decide if mutation should be a method by itself or done inside breeding

  /**
   * Spawns a new individual of type A.
   * @return
   */
  def spawn: A

  /**
   * Breeds two individuals of type A, with a mutation chance.
   * @param i1
   * @param i2
   * @param mutationRate
   * @return
   */
  def breed(i1: A, i2: A, mutationRate: Double): A

  /**
   * Decides how fit an individual is. The more positive, the better.
   * @param i
   * @return
   */
  def fitness(i: A): Double
}

class NaturalSelection[A](origin: Species[A]) {

  import NaturalSelection._

  type RankedIndividual = (A, Double)

  /**
   * Generates a population of size `n`.
   * @param n Size
   * @return
   */
  def populate(n: Int) = List.fill(n)(origin.spawn)

  //TODO: change the way this is being sorted, so it sorts in the correct order in one go
  /**
   * Ranks every individual in the population.
   * @param pop List of individuals
   * @return
   */
  def rank(pop: List[A]): List[RankedIndividual] =
    pop.map{ case x => (x, origin.fitness(x))}.sortBy(_._2).reverse


  //TODO: remove this function?
  /**
   * Culls a percentage of the population.
   * @param list List of individuals
   * @param perc Percentage of the population to be culled. Must be between 0 and 1.
   * @return
   */
  def cull[B](list: List[B], perc: Double) = {
    //    require(perc >= 0.0 && perc <= 1.0)
    val n = (1 - perc) * list.length
    list take n.toInt
  }

  /**
   * Breeds the next generation of individuals, based on the current surviving individuals
   * in the population.
   * @param rankedPop List of population with ranks
   * @param size Size of the new generation
   * @param mutationRate Mutation rate for breeding
   * @return New generation of individuals
   */
  def breed(rankedPop: List[RankedIndividual],
            size: Int,
            mutationRate: Double): List[A] = {
    val totalRank = rankedPop.map(x => x._2).sum
    val popProb = rankedPop map { case (i, r) => (i, r / totalRank)}
    List.fill(size) {
      val mate1 = choose(popProb)
      val mate2 = choose(popProb)
      origin.breed(mate1, mate2, mutationRate)
    }
  }

  /**
   * Tries to find the best possible solution.
   * @param popSize Population size
   * @param maxGen Number of generations
   * @param cullRate Culling rate of the population
   * @param mutationRate Mutation rate of new individuals
   * @return
   */
  def run(popSize: Int,
          maxGen: Int,
          cullRate: Double,
          mutationRate: Double): A = {
    //require(popSize > 0)

    def aux(pop: List[A], best: RankedIndividual, loop: Int): A = {
      println(s"Generation ${maxGen-loop}")
      if (loop == 0) best._1
      else {
        val rankedPop = rank(pop)
        val bestIndividual = rankedPop.head

        val culledPop = cull(rankedPop, cullRate)
        val newPop = breed(culledPop, popSize, mutationRate)

        aux(newPop, bestIndividual, loop - 1)
      }
    }

    val pop = populate(popSize)

    val rankedPop = rank(pop)
    println(rankedPop.map(x => x._2))
    val bestIndividual = rankedPop.head

    val culledPop = cull(rankedPop, cullRate)
    val newPop = breed(culledPop, popSize, mutationRate)

    aux(newPop, bestIndividual, maxGen - 1)
  }
}

//TODO: solve the problem of the size n of the solution
//class CMSpecies(cm: ComplexMachine, n: Int) extends Species[ComplexMachine] {
//  def spawn = RepairableSM.addRepairCM(cm, n)
//}

object NaturalSelection {
  /**
   * Chooses a random element from a list.
   * @param list List with tuples containing an element and the probability of choosing that element
   * @tparam A
   * @return
   */
  def choose[A](list: List[(A, Double)]): A = {
    def aux(u: Double, acc: Double, auxList: List[(A, Double)]): A = {
      val prob = auxList.head._2
//      println(auxList.length, prob)
      if (u < acc + prob) auxList.head._1 else aux(u, acc + prob, auxList.tail)
    }
    val r = rand.nextDouble()
    aux(r, 0.0, list)
  }

  //TODO: find better names for mixLists/mixListsWith and randomMixLists/randomMixListsWith
  //these functions will be used to mate machines
  /**
   * Randomly mix two lists.
   * The resulting list has the length of the smallest one.
   * Note that this mixes the beginning (with a random size `p`) of one of the lists with
   * the ending (with size `n-p`) of the other lists. The list that comes first is chosen at random.
   * @param l1 First list
   * @param l2 Second list
   * @return Mixed list
   */
  def mixLists[A](l1: List[A], l2: List[A]): List[A] =
    mixListsWith(l1, l2) { case x => x}

  /**
   * Randomly mix two lists, applying a function to each element of the result.
   * The resulting list has the length of the smallest list.
   * Note that this mixes the beginning (with a random size `p`) of one of the lists with
   * the ending (with size `n-p`) of the other lists. The list that comes first is chosen at random.
   * @param l1 First list
   * @param l2 Second list
   * @param f Map function
   * @return
   */
  def mixListsWith[A](l1: List[A], l2: List[A])
                     (f: A => A): List[A] = {
    val maxLength = Math.max(l1.length, l2.length)
    val cutOff = rand.nextInt(maxLength)
    val u = rand.nextDouble()

    if (u <= 0.5) {
      val start = l1.take(cutOff)
      val end = l2.drop(cutOff)
      List.concat(start, end)
    } else {
      val start = l2.take(cutOff)
      val end = l1.drop(cutOff)
      List.concat(start, end)
    } map f
  }

  /**
   * Randomly mix two lists.
   * The resulting list has the length of the smallest list.
   * Note that the elements of the resulting list are chosen randomly from either
   * one of the given lists.
   * @param l1
   * @param l2
   * @param f
   * @return
   */
  def randomMixListWith[A](l1: List[A], l2: List[A])
                          (f: A => A): List[A] = {
    val zipped = l1 zip l2
    val chosen = zipped map { case (x, y) => if (rand.nextDouble() < 0.5) x else y}
    chosen map f
  }

  /**
   * Randomly mix two lists, applying a function to each element of the result.
   * The resulting list has the length of the smallest list.
   * Note that the elements of the resulting list are chosen randomly from either
   * one of the given lists.
   * @param l1
   * @param l2
   * @return
   */
  def randomMixList[A](l1: List[A], l2: List[A]) = randomMixListWith(l1, l2) { case x => x}
}