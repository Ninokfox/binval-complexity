package binval

object Partitions {
  def foreach(items: Int, parts: Int, place: Array[Int], offset: Int)(fun: Array[Int] => Unit): Unit = {
    if (parts == 1) {
      place(offset) = items
      fun(place)
    } else {
      val li = offset + parts - 1
      var last = 0
      while (last <= items) {
        place(li) = last
        foreach(items - last, parts - 1, place, offset)(fun)
        last += 1
      }
    }
  }

  private def foreachChoiceInPartitionImpl(toChoose: Int, parts: Int,
                                           partition: Array[Int], choices: Array[Int],
                                           initialProbability: Double,
                                           choose: Choose[Double], partitionSum: Int,
                                           fun: Double => Unit): Unit = {
    if (parts == 1) {
      choices(0) = toChoose
      fun(initialProbability * choose(partition(0), toChoose))
    } else {
      val li = parts - 1
      val partSize = partition(li)
      var last = math.max(0, toChoose - partitionSum + partSize)
      val lastMax = math.min(partSize, toChoose)
      while (last <= lastMax) {
        choices(li) = last
        foreachChoiceInPartitionImpl(toChoose - last, parts - 1, partition, choices,
                                     initialProbability * choose(partSize, last),
                                     choose, partitionSum - partSize, fun)
        last += 1
      }
    }
  }

  def foreachChoiceInPartition(toChoose: Int, parts: Int,
                               partition: Array[Int], choices: Array[Int],
                               initialProbability: Double, choose: Choose[Double])
                              (fun: Double => Unit): Unit = {
    var i, partitionSum = 0
    while (i < parts) {
      partitionSum += partition(i)
      i += 1
    }
    foreachChoiceInPartitionImpl(toChoose, parts, partition, choices, initialProbability,
                                 choose, partitionSum, fun)
  }

  def foreachPair(n1: Int, n2: Int)(fun: (Int, Int) => Unit): Unit = {
    var i1 = 0
    while (i1 <= n1) {
      var i2 = 0
      while (i2 <= n2) {
        fun(i1, i2)
        i2 += 1
      }
      i1 += 1
    }
  }
}
