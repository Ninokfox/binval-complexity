package binval

import binval.binary.{Memory2NodeContainer, Memory3NodeContainer, NodeContainer, Solver}

object Main {
  def main(args: Array[String]): Unit = args(0) match {
    case "unrestricted" =>
      new UnrestrictedSolver(args(1).toInt, args.contains("r"), args.contains("v"))
    case "binary" =>
      def container(n: Int): NodeContainer = args(2) match {
        case "m2" => new Memory2NodeContainer(n)
        case "m3" => new Memory3NodeContainer(n)
        case etc => throw new IllegalArgumentException(s"Cannot process binary solver type '$etc'")
      }
      val a1 = args(1)
      if (a1(0) == '=') {
        println(new Solver(container(a1.tail.toInt), args.contains("v")).result)
      } else {
        var prev = 0.0
        for (n <- 2 to a1.toInt) {
          val t0 = System.currentTimeMillis()
          val result = new Solver(container(n), false).result
          val time = System.currentTimeMillis() - t0
          val delta = result - prev
          prev = result
          println(s"$n => $result (+$delta) in $time ms")
        }
      }
  }
}
