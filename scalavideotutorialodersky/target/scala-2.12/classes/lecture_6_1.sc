val xs = Array(1,2,3,44)

xs map (x => x*2)

val s = "Hello World"
s filter (c => c.isUpper)
s exists(c => c.isUpper)
s forall  (c => c.isUpper)
"SDVSDBV" forall  (c => c.isUpper)

val pair = xs zip s
val (x,y) = pair.unzip
x
y

s flatMap(c => List(c, "."))

xs.sum
xs.max
(1 to 5) flatMap (x => (1 to 10) map (y => (x, y)))

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map(xy => xy._1 * xy._2).sum
def isPrime (n:Int): Boolean = 
