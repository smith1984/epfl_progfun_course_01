val fruits = List ("apple", "orange","pinapple", "banana")
val nums = List (1,4,3,5,-6, 0, 67,12, 1, -3)

nums filter (x => x > 0)

nums filterNot (x => x > 0)

nums partition  (x => x > 0)

nums takeWhile  (x => x > 0)

nums dropWhile (x => x > 0)

nums span(x => x > 0)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => val (first, rest) = xs span(y => y == x)
    first::pack(rest)
}
val data = List("a", "a", "a", "b", "c", "c", "a")
pack(data)

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs).map (x => (x.head, x.length))

encode(data)

