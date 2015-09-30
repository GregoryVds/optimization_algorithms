object KnapDP {
    val items = Array((1,1), (6,2), (18,5), (22,6), (28,7))
    val C = 11

    def knapRecursive(k: Int, n: Int): Int = {
        println(s"Compute knapRecursive($k, $n)")
        if (n<0) 0
        else {
            val (vj, wj) = items(n)
            if (wj > k) knapRecursive(k,n-1)
            else knapRecursive(k,n-1).max(vj+knapRecursive(k-wj, n-1))
        }
    }

    def main(args: Array[String]) {
        println(knapRecursive(C, items.size-1))
    }
}
