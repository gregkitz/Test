val length = 4

val m =  Array.ofDim[Int](length, length)

var z = Array("123", "456", "789")

println(z.size)

val zInt = for {i1 <- z} yield i1.toInt

