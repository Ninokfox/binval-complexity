package binval.binary

class ExpectationAcc {
  var probability, expectation = 0.0
  def clear(): Unit = {
    probability = 0
    expectation = 0
  }
}

object ExpectationAcc {
  def addBestNode[T <: Node[T]](acc: ExpectationAcc, nodes: Array[T], p: Double): Unit = {
    var best = nodes(0)
    var i = 1
    while (i < nodes.length) {
      val curr = nodes(i)
      if (best.compare(curr) > 0) {
        best = curr
      }
      i += 1
    }
    if (best.isEvaluated) {
      acc.probability += p
      acc.expectation += best.expectedSteps * p
    }
  }
}
