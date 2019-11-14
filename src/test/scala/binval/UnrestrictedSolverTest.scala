package binval

import org.scalatest.{FlatSpec, Matchers}

class UnrestrictedSolverTest extends FlatSpec with Matchers {
  "The solver" should "compute G and row sums well" in {
    val solver = new UnrestrictedSolver(129, false, false)
    solver.average(32) shouldBe BigInt(23539723442L)
    solver.average(33) shouldBe BigInt(52223994403L)
    solver.average(34) shouldBe BigInt(104854453980L)

    solver.G(34, 25) shouldBe BigInt(316603932L)
    solver.G(33, 19) shouldBe BigInt(4981911328L)

    solver.average(129) shouldBe BigInt("5498740584177903373490504348364250084067")
    solver.G(129, 35) shouldBe BigInt("352221856278812896016347417069556")
  }
}
