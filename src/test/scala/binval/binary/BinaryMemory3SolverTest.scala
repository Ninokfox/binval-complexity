package binval.binary

import org.scalatest.{FlatSpec, Matchers}

class BinaryMemory3SolverTest extends FlatSpec with Matchers {
  private def container(n: Int): NodeContainer = new Memory3NodeContainer(n)

  "The solver" should "find the final results correctly" in {
    new Solver(container(2), false).result shouldBe 2.0
    new Solver(container(8), false).result shouldBe 8.853198722782997
  }
}
