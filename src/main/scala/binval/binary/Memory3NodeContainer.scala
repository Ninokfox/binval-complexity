package binval.binary

import binval.Partitions

class Memory3NodeContainer(n: Int) extends NodeContainer(n) {
  override type N = MyNode

  override def nodeDescriptionSize: Int = 8
  override def getNode(a: Array[Int]): MyNode = getNode(a(0), a(1), a(2), a(3), a(4), a(5), a(6), a(7))

  private val cache = Array.fill(n / 2 + 1, n / 2 + 1, n / 2 + 1, n + 1)(Array.ofDim[MyNode](n + 1, n + 1, n + 1))
  private val cache0 = Array.ofDim[MyNode](n + 1, n + 1, n + 1, n + 1)

  Partitions.foreach(n, 8, new Array(8), 0)(makeNode)
  Partitions.foreach(n, 8, new Array(8), 0)(a => getNode(a).populateDependencies())

  private final def getNode(a0: Int, a1: Int, a2: Int, a3: Int, a4: Int, a5: Int, a6: Int, a7: Int): MyNode = {
    if (a1 > a2 || a1 == a2 && a5 > a6) {        // First, sort args anti-lexicographically by permutations
      getNode(a0, a2, a1, a3, a4, a6, a5, a7)
    } else if (a1 > a4 || a1 == a4 && a3 > a6) {
      getNode(a0, a4, a2, a6, a1, a5, a3, a7)
    } else if (a2 > a4 || a2 == a4 && a3 > a5) {
      getNode(a0, a1, a4, a5, a2, a3, a6, a7)
    } else if (a1 + a3 + a5 + a7 == 0) {         // If, after sorting, the last string (which can be all-zeros) is all-zeros, return the special node
      cache0(a0)(a2)(a4)(a6)
    } else if (a0 > a1 || a0 == a1 && (a2 > a3 || a2 == a3 && (a4 > a5 || a4 == a5 && a6 > a7))) { // If the last string is nonzero, we can try inverting it and kill some more symmetries
      getNode(a1, a0, a3, a2, a5, a4, a7, a6)
    } else if (a0 > a3 || a0 == a3 && a4 > a7) {
      getNode(a3, a2, a1, a0, a7, a6, a5, a4)
    } else if (a0 > a7 || a0 == a7 && (a1 > a6 || a1 == a6 && (a2 > a5 && a2 == a5 && a3 > a4))) {
      getNode(a7, a6, a5, a4, a3, a2, a1, a0)
    } else {
      cache(a0)(a1)(a2)(a3)(a4)(a5)(a6)
    }
  }

  private final def makeNode(a0: Int, a1: Int, a2: Int, a3: Int, a4: Int, a5: Int, a6: Int, a7: Int): Unit = {
    if (a1 > a2 || a1 == a2 && a5 > a6) {
      makeNode(a0, a2, a1, a3, a4, a6, a5, a7)
    } else if (a1 > a4 || a1 == a4 && a3 > a6) {
      makeNode(a0, a4, a2, a6, a1, a5, a3, a7)
    } else if (a2 > a4 || a2 == a4 && a3 > a5) {
      makeNode(a0, a1, a4, a5, a2, a3, a6, a7)
    } else if (a1 + a3 + a5 + a7 == 0) {
      if (cache0(a0)(a2)(a4)(a6) == null) {
        val node = new MyNode(a0, a1, a2, a3, a4, a5, a6)
        cache0(a0)(a2)(a4)(a6) = node
        allMyNodes += node
      }
    } else if (a0 > a1 || a0 == a1 && (a2 > a3 || a2 == a3 && (a4 > a5 || a4 == a5 && a6 > a7))) {
      makeNode(a1, a0, a3, a2, a5, a4, a7, a6)
    } else if (a0 > a3 || a0 == a3 && a4 > a7) {
      makeNode(a3, a2, a1, a0, a7, a6, a5, a4)
    } else if (a0 > a7 || a0 == a7 && (a1 > a6 || a1 == a6 && (a2 > a5 && a2 == a5 && a3 > a4))) {
      makeNode(a7, a6, a5, a4, a3, a2, a1, a0)
    } else {
      if (cache(a0)(a1)(a2)(a3)(a4)(a5)(a6) == null) {
        val node = new MyNode(a0, a1, a2, a3, a4, a5, a6)
        cache(a0)(a1)(a2)(a3)(a4)(a5)(a6) = node
        allMyNodes += node
      }
    }
  }

  private final def makeNode(a: Array[Int]): Unit = makeNode(a(0), a(1), a(2), a(3), a(4), a(5), a(6), a(7))

  final class MyNode(val a0: Int, val a1: Int, val a2: Int, val a3: Int, val a4: Int, val a5: Int, val a6: Int) extends Node[MyNode] {
    val a7: Int = n - a0 - a1 - a2 - a3 - a4 - a5 - a6

    override protected def self: MyNode = this
    override def description: String = s"($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7)"

    override def accumulateOverDependencies[OuterAcc, InnerAcc](outer: OuterAcc,
                                                                inner: InnerAcc,
                                                                resetInner: InnerAcc => Unit,
                                                                populateInner: (InnerAcc, Array[MyNode], Double) => Unit,
                                                                populateOuter: (OuterAcc, InnerAcc, Int, Int, Int, Int) => Unit): Unit = {
      val choiceS, choiceD = new Array[Int](4)
      val nodeArray = new Array[MyNode](3)

      // Try crossing args 0 and 1
      val partition0167 = Array(a0, a1, a6, a7)
      val partition2345 = Array(a2, a3, a4, a5)
      Partitions.foreachPair(a0 + a1 + a6 + a7, a2 + a3 + a4 + a5) { (dS, dD) =>
        val probDenominator = 1 / choose(a0 + a1 + a6 + a7, dS) / choose(a2 + a3 + a4 + a5, dD)
        resetInner(inner)
        Partitions.foreachChoiceInPartition(dS, 4, partition0167, choiceS, probDenominator, choose) { probLeft =>
          Partitions.foreachChoiceInPartition(dD, 4, partition2345, choiceD, probLeft, choose) { prob =>
            val g0 = choiceS(0)
            val g1 = choiceS(1)
            val g2 = choiceD(0)
            val g3 = choiceD(1)
            val g4 = choiceD(2)
            val g5 = choiceD(3)
            val g6 = choiceS(2)
            val g7 = choiceS(3)
            nodeArray(0) = getNode(a0 - g0 + a1 - g1, g0 + g1, a2 - g2 + a3 - g3, g2 + g3, g4 + g5, a4 - g4 + a5 - g5, g6 + g7, a6 - g6 + a7 - g7)
            nodeArray(1) = getNode(a0 - g0 + a2 - g2, g0 + g2, a1 - g1 + a3 - g3, g1 + g3, g4 + g6, a4 - g4 + a6 - g6, g5 + g7, a5 - g5 + a7 - g7)
            nodeArray(2) = getNode(a0 - g0 + g4, g0 + a4 - g4, a1 - g1 + g5, g1 + a5 - g5, a2 - g2 + g6, g2 + a6 - g6, a3 - g3 + g7, g3 + a7 - g7)
            populateInner(inner, nodeArray, prob)
          }
        }
        populateOuter(outer, inner, dS, dD, 0, 1)
      }

      // Try crossing args 0 and 2
      val partition0257 = Array(a0, a2, a5, a7)
      val partition1346 = Array(a1, a3, a4, a6)
      Partitions.foreachPair(a0 + a2 + a5 + a7, a1 + a3 + a4 + a6) { (dS, dD) =>
        val probDenominator = 1 / choose(a0 + a2 + a5 + a7, dS) / choose(a1 + a3 + a4 + a6, dD)
        resetInner(inner)
        Partitions.foreachChoiceInPartition(dS, 4, partition0257, choiceS, probDenominator, choose) { probLeft =>
          Partitions.foreachChoiceInPartition(dD, 4, partition1346, choiceD, probLeft, choose) { prob =>
            val g0 = choiceS(0)
            val g1 = choiceD(0)
            val g2 = choiceS(1)
            val g3 = choiceD(1)
            val g4 = choiceD(2)
            val g5 = choiceS(2)
            val g6 = choiceD(3)
            val g7 = choiceS(3)
            nodeArray(0) = getNode(a0 - g0 + a1 - g1, g0 + g1, a2 - g2 + a3 - g3, g2 + g3, g4 + g5, a4 - g4 + a5 - g5, g6 + g7, a6 - g6 + a7 - g7)
            nodeArray(1) = getNode(a0 - g0 + a2 - g2, g0 + g2, a1 - g1 + a3 - g3, g1 + g3, g4 + g6, a4 - g4 + a6 - g6, g5 + g7, a5 - g5 + a7 - g7)
            nodeArray(2) = getNode(a0 - g0 + g4, g0 + a4 - g4, a1 - g1 + g5, g1 + a5 - g5, a2 - g2 + g6, g2 + a6 - g6, a3 - g3 + g7, g3 + a7 - g7)
            populateInner(inner, nodeArray, prob)
          }
        }
        populateOuter(outer, inner, dS, dD, 0, 2)
      }

      // Try crossing args 1 and 2
      val partition0347 = Array(a0, a3, a4, a7)
      val partition1256 = Array(a1, a2, a5, a6)
      Partitions.foreachPair(a0 + a3 + a4 + a7, a1 + a2 + a5 + a6) { (dS, dD) =>
        val probDenominator = 1 / choose(a0 + a3 + a4 + a7, dS) / choose(a1 + a2 + a5 + a6, dD)
        resetInner(inner)
        Partitions.foreachChoiceInPartition(dS, 4, partition0347, choiceS, probDenominator, choose) { probLeft =>
          Partitions.foreachChoiceInPartition(dD, 4, partition1256, choiceD, probLeft, choose) { prob =>
            val g0 = choiceS(0)
            val g1 = choiceD(0)
            val g2 = choiceD(1)
            val g3 = choiceS(1)
            val g4 = choiceS(2)
            val g5 = choiceD(2)
            val g6 = choiceD(3)
            val g7 = choiceS(3)
            nodeArray(0) = getNode(a0 - g0 + a1 - g1, g0 + g1, g2 + g3, a2 - g2 + a3 - g3, a4 - g4 + a5 - g5, g4 + g5, g6 + g7, a6 - g6 + a7 - g7)
            nodeArray(1) = getNode(a0 - g0 + g2, g0 + a2 - g2, a1 - g1 + g3, g1 + a3 - g3, a4 - g4 + g6, g4 + a6 - g6, a5 - g5 + g7, g5 + a7 - g7)
            nodeArray(2) = getNode(a0 - g0 + a4 - g4, g0 + g4, a1 - g1 + a5 - g5, g1 + g5, g2 + g6, a2 - g2 + a6 - g6, g3 + g7, a3 - g3 + a7 - g7)
            populateInner(inner, nodeArray, prob)
          }
        }
        populateOuter(outer, inner, dS, dD, 1, 2)
      }
    }

    override def compare(that: MyNode): Int = {
      if (expectedSteps != that.expectedSteps) {
        java.lang.Double.compare(expectedSteps, that.expectedSteps)
      } else if (a0 != that.a0) {
        java.lang.Integer.compare(a0, that.a0)
      } else if (a1 != that.a1) {
        java.lang.Integer.compare(a1, that.a1)
      } else if (a2 != that.a2) {
        java.lang.Integer.compare(a2, that.a2)
      } else if (a3 != that.a3) {
        java.lang.Integer.compare(a3, that.a3)
      } else if (a4 != that.a4) {
        java.lang.Integer.compare(a4, that.a4)
      } else if (a5 != that.a5) {
        java.lang.Integer.compare(a5, that.a5)
      } else if (a6 != that.a6) {
        java.lang.Integer.compare(a6, that.a6)
      } else {
        java.lang.Integer.compare(a7, that.a7)
      }
    }
  }
}
