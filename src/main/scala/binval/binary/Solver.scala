package binval.binary

import binval.Partitions

import scala.collection.mutable

class Solver(container: NodeContainer, verboseIntermediate: Boolean) {
  private val n = container.n
  private val choose = container.choose
  private val desc = container.nodeDescriptionSize
  private val heap = new mutable.TreeSet[container.N]()

  // Initialize the definitely zero nodes
  Partitions.foreach(n, desc / 2, new Array[Int](desc), desc / 2) { arr =>
    val node = container.getNode(arr)
    node.isEvaluated = true
    node.expectedSteps = 1
    if (verboseIntermediate) {
      println(s"E${node.description} = ${node.expectedSteps}")
    }
    node.dependOnMe.foreach(n => if (!n.isEvaluated) n.needRecomputing = true)
    heap += node
  }

  // Run Dijkstra
  while (heap.nonEmpty) {
    val best = heap.head
    heap -= best
    if (verboseIntermediate) {
      println(s"E${best.description} = ${best.expectedSteps} by ${best.dS}, ${best.dD} on ${best.arg0}, ${best.arg1}")
    }
    best.isEvaluated = true
    for (n <- best.dependOnMe) {
      if (!n.isEvaluated) {
        heap -= n
        n.needRecomputing = true
        n.evaluate()
        heap += n
      }
    }
  }

  // Compute the expected time
  private val place = new Array[Int](desc)
  val result: Double = (0 to n).map(i => {
    place(0) = n - i
    place(desc - 1) = i
    choose(n, i) / math.pow(2, n) * container.getNode(place).expectedSteps
  }).sum
}
