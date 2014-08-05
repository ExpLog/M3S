import m3s._
import m3s.machines._
import m3s.machines.connectors.Parallel
import m3s.machines.output.LinearOutput
import m3s.markov.DenseMarkovChain
import org.scalacheck.Gen._
import org.scalacheck.Gen

/**
 * Contains ScalaCheck generators for all classes in [[m3s]] package.
 */
object Generators {
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
  def squareMatrixOfN(n: Int): Gen[Matrix] = vectorOfN(n, vectorOfN(n, choose[Double](0,10)))

  /**
   * Generates a square matrix of random dimension between 1 and 10.
   * @return
   */
  def squareMatrix: Gen[Matrix] = for(i <- choose[Int](1,10); mtx <- squareMatrixOfN(i)) yield mtx

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

  def markovChainOfN(n: Int) = for(mtx <- rowNormMatrixOfN(n)) yield DenseMarkovChain(mtx)

  def markovChain = for(mtx <- rowNormMatrix) yield DenseMarkovChain(mtx)

  def linearFunction: Gen[Int => Double] =
    for(d <- choose[Double](1,10)) yield {
      val f = {x: Int => x*d}
      f
    }

  def machine: Gen[Machine] =
    Gen.lzy(oneOf(simpleMachine, complexMachine))

  def simpleMachine: Gen[SimpleMachine] =
    for(mc <- markovChain; i <- choose[Int](0,mc.nStates-1)) yield SimpleMachine(mc,i,LinearOutput(1.0, 0.0))

  def complexMachine: Gen[ComplexMachine] = Gen.lzy{
    for{
      n <- choose(1,2)
      lm <- listOfN(n, machine)
    } yield ComplexMachine(lm)(Parallel)
  }
}
