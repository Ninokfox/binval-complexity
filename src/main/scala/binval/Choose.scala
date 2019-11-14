package binval

import scala.reflect.ClassTag

class Choose[@specialized(Specializable.BestOfBreed) T](N: Int)(implicit num: Numeric[T], arr: ClassTag[T]) {
  private val data = new Array[Array[T]](N + 1)
  private val one = num.fromInt(1)

  data(0) = Array(one)
  for (i <- 1 to N) {
    val prev = data(i - 1)
    data(i) = Array.tabulate(i + 1)(j => if (j == 0 || j == i) one else num.plus(prev(j), prev(j - 1)))
  }

  def apply(n: Int, k: Int): T = data(n)(k)
}
