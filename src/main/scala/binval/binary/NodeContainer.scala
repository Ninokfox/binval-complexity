package binval.binary

import binval.Choose

import scala.collection.mutable

abstract class NodeContainer(val n: Int) {
  type N <: Node[N]

  protected val allMyNodes = new mutable.ArrayBuffer[N]()
  val choose: Choose[Double] = new Choose(n)

  def nodeDescriptionSize: Int

  def getNode(description: Array[Int]): N
  def allNodes: Seq[N] = allMyNodes
}
