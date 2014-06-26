import m3s._
import m3s.machines._
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
  def squareMatrixOfN(n: Int): Gen[Matrix] = vectorOfN(n, vectorOfN(n, choose(0,100)))

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

  def machine(f: Seq[Machine] => State): Gen[Machine] = oneOf(simpleMachine, complexMachine(f))

  def simpleMachine: Gen[SimpleMachine] =
    for(mc <- markovChain; i <- chooseNum(0,mc.nStates, mc.nStates)) yield SimpleMachine(mc,i)

  def complexMachine(f: Seq[Machine] => State): Gen[ComplexMachine] =
    for{
      n <- choose(1,10)
      lm <- listOfN(n, machine(f))
    } yield ComplexMachine(lm:_*)(f)

  def performanceMachine(p: State => Double)(f: Seq[Machine] => State): Gen[PerformanceMachine] =
    for(m <- machine(f)) yield PerformanceMachine(m)(p)
}
