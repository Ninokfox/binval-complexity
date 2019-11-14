package binval.binary

import org.scalatest.{FlatSpec, Matchers}

class BinaryMemory2SolverTest extends FlatSpec with Matchers {
  private def container(n: Int): NodeContainer = new Memory2NodeContainer(n)

  "The solver" should "find the final results correctly" in {
    new Solver(container(2), false).result shouldBe 2.0
    new Solver(container(20), false).result shouldBe 29.813320310706573
  }
}
