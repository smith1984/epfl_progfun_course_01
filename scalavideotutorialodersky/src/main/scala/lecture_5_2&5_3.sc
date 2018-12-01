def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]):List[Int] =
      (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (x < y) x :: merge (xs1, ys)
          else y :: merge (xs, ys1)
      }

    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

val nums = List (1,4,3,5,6, 67,12, 1,3)
msort(nums)

def msort_[T](xs: List[T])(lt: (T,T)=> Boolean): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]):List[T] =
      (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (lt(x,y)) x :: merge (xs1, ys)
          else y :: merge (xs, ys1)
      }

    val (fst, snd) = xs splitAt n
    merge(msort_(fst)(lt), msort_(snd)(lt))
  }
}
msort_(List (1,4,3,5,6, 67,12, 1,3))((x, y) => x < y)

val fruits = List ("apple", "orange","pinapple", "banana")
msort_(fruits)((x, y) => x.compareTo(y) < 0 )

def msort_2[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]):List[T] =
      (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x,y)) x :: merge (xs1, ys)
          else y :: merge (xs, ys1)
      }

    val (fst, snd) = xs splitAt n
    merge(msort_2(fst), msort_2(snd))
  }
}

msort_2(List (1,4,3,5,6, 67,12, 1,3))

msort_2(fruits)

