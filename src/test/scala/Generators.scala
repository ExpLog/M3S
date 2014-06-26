import m3s._
import m3s.machines._
import org.scalacheck.Gen._
import org.scalacheck.Gen

/**
 * Contains ScalaCheck generators for all classes in [[m3s]] package.
 */
object Generators {
  /**
   * A simple function that sums the structure of a sequence of machines,
   * to be used in some generators.
   * @param sm
   * @return
   */
  def sum(sm: Seq[Machine]): State = sm.map(m => m.structure).sum

  /**
   * Generates a vector of the given length.
   * @param n Desired list length.
   * @param g Generator of type T.
   * @tparam T Type to generate.
   * @return
   */
  def vectorOfN[T](n: Int, g: => Gen[T]) = containerOfN[Vector,T](n,g)

  /**
   * Generates a square matrix of size n.
   * @param n Desired matrix dimension.
   * @return
   */
  def squareMatrixOfN(n: Int): Gen[Matrix] = vectorOfN(n, vectorOfN(n, chooseNum[Double](0,100)))

  /**
   * Generates a square matrix of random dimension between 1 and 150.
   * @return
   */
  def squareMatrix: Gen[Matrix] = for(i <- choose(1,150); mtx <- squareMatrixOfN(i)) yield mtx

  /**
   * Generates a square row normalized matrix of dimension N.
   * @param n Desired matrix dimension.
   * @return
   */
  def rowNormMatrixOfN(n: Int) = for(mtx <- squareMatrixOfN(n)) yield matrixToRowNormMatrix(mtx)

  /**
   * Generates a square row normalized matrix of random dimension between 1 and 150.
   * @return
   */
  def rowNormMatrix = for(mtx <- squareMatrix) yield matrixToRowNormMatrix(mtx)

  def markovChainOfN(n: Int) = for(mtx <- rowNormMatrixOfN(n)) yield MarkovChain(mtx)

  def markovChain = for(mtx <- rowNormMatrix) yield MarkovChain(mtx)

  def linearFunction: Gen[Int => Double] =
    for(d <- chooseNum[Double](1,10)) yield {
      val f = {x: Int => x*d}
      f
    }

  def machine: Gen[Machine] =
    Gen.lzy(oneOf(simpleMachine, complexMachine, performanceMachine))

  def simpleMachine: Gen[SimpleMachine] =
    for(mc <- markovChain; i <- chooseNum[Int](0,mc.nStates, mc.nStates)) yield SimpleMachine(mc,i)

  def complexMachine: Gen[ComplexMachine] =
    for{
      n <- choose(1,10)
      lm <- listOfN(n, machine)
    } yield ComplexMachine(lm:_*)(sum)

  def performanceMachine: Gen[PerformanceMachine] =
    for{
      m <- machine
      lf <- linearFunction
    } yield PerformanceMachine(m)(lf)
}
