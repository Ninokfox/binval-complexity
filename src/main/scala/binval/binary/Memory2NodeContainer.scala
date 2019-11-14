package binval.binary

import binval.Partitions

class Memory2NodeContainer(n: Int) extends NodeContainer(n) {
  override type N = MyNode

  override def nodeDescriptionSize: Int = 4
  override def getNode(a: Array[Int]): MyNode = getNode(a(0), a(1), a(2), a(3))

  private val cache = Array.ofDim[MyNode](n / 2 + 1, n / 2 + 1, n + 1)
  private val cache0 = Array.ofDim[MyNode](n + 1, n + 1)

  Partitions.foreach(n, 4, new Array(4), 0)(makeNode)
  Partitions.foreach(n, 4, new Array(4), 0)(a => getNode(a).populateDependencies())

  private final def getNode(a0: Int, a1: Int, a2: Int, a3: Int): MyNode = {
    if (a1 > a2) {
      getNode(a0, a2, a1, a3)
    } else if (a1 + a3 == 0) {
      cache0(a0)(a2)
    } else if (a0 > a1 || a0 == a1 && a2 > a3) {
      getNode(a1, a0, a3, a2)
    } else if (a0 > a3) {
      getNode(a3, a2, a1, a0)
    } else {
      cache(a0)(a1)(a2)
    }
  }

  private final def makeNode(a0: Int, a1: Int, a2: Int, a3: Int): Unit = {
    if (a1 > a2) {
      makeNode(a0, a2, a1, a3)
    } else if (a1 + a3 == 0) {
      if (cache0(a0)(a2) == null) {
        val node = new MyNode(a0, a1, a2)
        cache0(a0)(a2) = node
        allMyNodes += node
      }
    } else if (a0 > a1 || a0 == a1 && a2 > a3) {
      makeNode(a1, a0, a3, a2)
    } else if (a0 > a3) {
      makeNode(a3, a2, a1, a0)
    } else {
      if (cache(a0)(a1)(a2) == null) {
        val node = new MyNode(a0, a1, a2)
        cache(a0)(a1)(a2) = node
        allMyNodes += node
      }
    }
  }

  private final def makeNode(a: Array[Int]): Unit = makeNode(a(0), a(1), a(2), a(3))

  final class MyNode(val a0: Int, val a1: Int, val a2: Int) extends Node[MyNode] {
    val a3: Int = n - a0 - a1 - a2

    override protected def self: MyNode = this
    override def description: String = s"($a0, $a1, $a2, $a3)"

    override def accumulateOverDependencies[OuterAcc, InnerAcc](outer: OuterAcc,
                                                                inner: InnerAcc,
                                                                resetInner: InnerAcc => Unit,
                                                                populateInner: (InnerAcc, Array[MyNode], Double) => Unit,
                                                                populateOuter: (OuterAcc, InnerAcc, Int, Int, Int, Int) => Unit): Unit = {
      val partition03 = Array(a0, a3)
      val partition12 = Array(a1, a2)
      val choice03, choice12 = new Array[Int](2)
      val nodeArray = new Array[MyNode](2)
      Partitions.foreachPair(a0 + a3, a1 + a2) { (dS, dD) =>
        val probDenominator = 1 / choose(a0 + a3, dS) / choose(a1 + a2, dD)
        resetInner(inner)
        Partitions.foreachChoiceInPartition(dS, 2, partition03, choice03, probDenominator, choose) { probLeft =>
          Partitions.foreachChoiceInPartition(dD, 2, partition12, choice12, probLeft, choose) { prob =>
            val g0 = choice03(0)
            val g1 = choice12(0)
            val g2 = choice12(1)
            val g3 = choice03(1)
            nodeArray(0) = getNode(a0 - g0 + a1 - g1, g0 + g1, g2 + g3, a2 - g2 + a3 - g3)
            nodeArray(1) = getNode(a0 - g0 + g2, g0 + a2 - g2, a1 - g1 + g3, g1 + a3 - g3)
            populateInner(inner, nodeArray, prob)
          }
        }
        populateOuter(outer, inner, dS, dD, 0, 1)
      }
    }

    override def compare(that: MyNode): Int = {
      if (expectedSteps != that.expectedSteps) {
        java.lang.Double.compare(expectedSteps, that.expectedSteps)
      } else if (a0 != that.a0) {
        java.lang.Integer.compare(a0, that.a0)
      } else if (a1 != that.a1) {
        java.lang.Integer.compare(a1, that.a1)
      } else {
        java.lang.Integer.compare(a2, that.a2)
      }
    }
  }
}
