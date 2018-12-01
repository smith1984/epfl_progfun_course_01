val n = 7


def isPrime(i: Int):Boolean = {
  (2 until i) forall (d => i % d != 0)

}

def scalapProduct(xs: List[Double], ys: List[Double]): Double =
{
  (for ((x,y) <- xs zip ys) yield x*y).sum
}

(1 until n) flatMap (i =>
  (1 until i) map (j => (i, j))) filter ( pair =>
  isPrime(pair._1 + pair._2))