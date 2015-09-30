object KnapDPMemo {
    val items = Array((1,1), (6,2), (18,5), (22,6), (28,7))
    val C = 11
    val cache = collection.mutable.Map.empty[(Int,Int),Int]

    def knapRecursive(k: Int, n: Int): Int = {
        println(s"Compute knapRecursive($k, $n)")
        if (n<1) 0
        else {
            val (vj, wj) = items(n-1)
            if (wj > k) knapRecursive(k,n-1)
            else memoKnapRecursive(k,n-1).max(vj+memoKnapRecursive(k-wj, n-1))
        }
    }

    def memoKnapRecursive(k: Int, n: Int): Int = {
        cache.getOrElseUpdate((k,n), knapRecursive(k,n))
    }

    def main(args: Array[String]) {
        println(knapRecursive(C, items.size))
    }
}
