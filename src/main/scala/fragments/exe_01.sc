import _root_.java.lang.System

val test_lst = (1 to 10000000).toList
val test_set = test_lst.toSet
val result = test_set.contains(2)


// TODO: load values from file
// TODO: google aufgabe


def time[R](block: => R): R = {
  val t0 = System.nanoTime
  val result = block    // call-by-name
  val t1 = System.nanoTime
  println("Elapsed time: " + (t1 - t0) / 1e-6 + "ms")
  result
}

// 4.
def get_summing_pair_for(target_value: Int, values: List[Int]): Int = {
  val value_set = values.toSet
  val result_lst = for{
    x <- values
    y = target_value - x
    if value_set contains y
  } yield x*y
  result_lst.head
}

val result = time {
  get_summing_pair_for(3, test_lst)
}

// 1.
def get_summing_pair(target_value: Int, values: List[Int]): Int = {
  val value_set = values.toSet

  def search(target_value: Int, head: Int, tail: List[Int], value_set: Set[Int]): List[Int] = {
    val other = target_value - head
    if (value_set.contains(other)) List(head, other)
    else search(target_value, tail.head, tail.tail, value_set)
  }
  search(target_value, values.head, values.tail, value_set).product
}

val result = time {
  get_summing_pair(3, test_lst)
}

val result = time {
  get_summing_pair(3, test_lst)
}


// Should be less efficient
// 3.
def get_summing_pair_map_find(target_value: Int, values: List[Int]): Int = { // TODO efficiency?
  val value_set = values.toSet
  val result = values map {x => (x, target_value - x)} find {x => value_set contains x._2}
  val (x,y) = result.get
  x*y
}

val result = time {
  get_summing_pair_map_find(3, test_lst)
}

