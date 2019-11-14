package binval.binary

import scala.collection.mutable

abstract class Node[Self <: Node[Self]] extends Ordered[Self] {
  var isEvaluated, needRecomputing: Boolean = false
  var expectedSteps: Double = Double.PositiveInfinity
  var dS, dD, arg0, arg1 = -1
  final val dependOnMe = new mutable.HashSet[Self]()

  protected def self: Self

  def description: String

  def accumulateOverDependencies[OuterAcc, InnerAcc](outer: OuterAcc, inner: InnerAcc,
                                                     resetInner: InnerAcc => Unit,
                                                     populateInner: (InnerAcc, Array[Self], Double) => Unit,
                                                     populateOuter: (OuterAcc, InnerAcc, Int, Int, Int, Int) => Unit): Unit

  def populateDependencies(): Unit = {
    accumulateOverDependencies[Unit, Self]((), self, _ => (), Node.addAccumulatorAsAChild, (_, _, _, _, _, _) => ())
  }


  def evaluate(): Double = {
    assert(needRecomputing)
    expectedSteps = Double.PositiveInfinity
    dS = -1
    dD = -1
    arg0 = -1
    arg1 = -1
    accumulateOverDependencies[Self, ExpectationAcc](
      self,
      new ExpectationAcc,
      _.clear(),
      ExpectationAcc.addBestNode[Self],
      (out, acc, dS, dD, arg0, arg1) => {
        val exp = (acc.expectation + 1) / acc.probability
        if (exp < out.expectedSteps) {
          out.expectedSteps = exp
          out.dS = dS
          out.dD = dD
          out.arg0 = arg0
          out.arg1 = arg1
        }
      }
    )
    needRecomputing = false
    expectedSteps
  }
}

object Node {
  private def addAccumulatorAsAChild[T <: Node[T]](acc: T, nodes: Array[T], ignored: Double): Unit = {
    nodes.foreach(_.dependOnMe += acc)
  }
}
