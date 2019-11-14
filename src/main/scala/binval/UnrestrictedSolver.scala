package binval

class UnrestrictedSolver(N: Int, showRowSums: Boolean, showGValues: Boolean) {
  private[this] val choose = new Choose[BigInt](N)

  // E(n, d) = g(n)(d) / choose(n, d)

  private[this] val g = Array.tabulate(N + 1)(i => new Array[BigInt](i + 1))
  locally {
    for (n <- 1 to N) {
      g(n)(0) = 1
      g(n)(n) = 0
      if (showGValues) println(s"g($n)(0) <- ${g(n)(0)}")
      for (d <- 1 until n) {
        val s = n / 2   // experimentally shown to give minimum results among s in [1; n - 1], need to prove this.
        var sum = choose(n, d) // to match the "1 + ..." in the formula for expectations
        val u = math.min(d, n - d)
        var l = 0
        while (l <= u) {
          sum += (g(s)(s - l) * choose(n - s, u - l)).max(g(n - s)(u - l) * choose(s, l))
          l += 1
        }
        g(n)(d) = sum
        if (showGValues) println(s"g($n)($d) <- $sum = ${sum.toDouble / choose(n, d).toDouble}")
      }
      if (showGValues) println(s"g($n)($n) <- ${g(n)(n)}")
      if (showRowSums && showGValues) println("----")
      val res = g(n).sum
      if (showRowSums) println(s"Average at $n: $res")
      if (showRowSums && showGValues) println()
    }
  }

  def G(n: Int, d: Int): BigInt = g(n)(d)
  def average(n: Int): BigInt = g(n).sum
}
