package binval

import org.scalatest.{FlatSpec, Matchers}

class ChooseTest extends FlatSpec with Matchers {
  "Choose" should "produce expected results for small arguments" in {
    val choose = new Choose[BigInt](4)
    choose(0, 0) shouldBe BigInt(1)
    choose(1, 0) shouldBe BigInt(1)
    choose(1, 1) shouldBe BigInt(1)
    choose(2, 0) shouldBe BigInt(1)
    choose(2, 1) shouldBe BigInt(2)
    choose(2, 2) shouldBe BigInt(1)
    choose(3, 0) shouldBe BigInt(1)
    choose(3, 1) shouldBe BigInt(3)
    choose(3, 2) shouldBe BigInt(3)
    choose(3, 3) shouldBe BigInt(1)
    choose(4, 0) shouldBe BigInt(1)
    choose(4, 1) shouldBe BigInt(4)
    choose(4, 2) shouldBe BigInt(6)
    choose(4, 3) shouldBe BigInt(4)
    choose(4, 4) shouldBe BigInt(1)
  }
}
