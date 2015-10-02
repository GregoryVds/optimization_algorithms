import scala.io.Source
import scala.collection.mutable.PriorityQueue
import scala.util.control.Breaks._

class KnapsackBranchAndBound(items: Array[(Int,Int,Double,Int)], capacity: Int) {
    val value   = items.map(x => x._1)
    val weight  = items.map(x => x._2)
    val ratio   = items.map(x => x._3)
    val index   = items.map(x => x._4)
    val n       = items.length

    def partialWeight(candidate: Array[Int]) =
        (0 until candidate.length).map(i => candidate(i)*weight(i)).sum

    def candidateValue(candidate: Array[Int]) =
        (0 until candidate.length).map(i => candidate(i)*value(i)).sum

    def root() = (optimisticHeuristic(Array[Int]()), 0, Array[Int]())

    def exceedCapacity(candidate: Array[Int]) =
        partialWeight(candidate) > capacity

    def children(candidate: (Double, Int, Array[Int])) : Seq[(Double, Int, Array[Int])] =
        if (candidate._3.length == n) Seq()
        else {
            val leftChild = (optimisticHeuristic(candidate._3), candidate._2+value(candidate._3.length), candidate._3:+1)
            val rightChild = (optimisticHeuristic(candidate._3), candidate._2, candidate._3:+0)
            Seq(leftChild, rightChild)
        }

    def output(candidate: Array[Int]) {
        var solution = Array.fill(n){0}
        for (i <- 0 until candidate.length) {
            solution(index(i)) = candidate(i)
        }
        println(solution.mkString(" "))
    }

    // CurrentPartialWeight + PartialItemsKnapSack for remaining capacity/items
    def optimisticHeuristic(candidate: Array[Int]) : Double = {
        var currentValue : Double  = candidateValue(candidate)
        var currentWeight : Double = partialWeight(candidate)
        breakable {
            for (i <- candidate.length until items.length) {
                if ((weight(i) + currentWeight) < capacity) {
                    currentWeight += weight(i)
                    currentValue  += value(i)
                } else {
                    val capacityLeft = (capacity - currentWeight)
                    val fractionOfLastItem = capacityLeft/weight(i)
                    currentValue += fractionOfLastItem*value(i)
                    break
                }
            }
        }
        currentValue
    }
}

object App {
    def main(args: Array[String]) {
        // Read all lines from input file
        val lines       = Source.fromFile(args(0)).getLines

        // Extract capacity and number of items
        val firstLine   = lines.next()
        val itemsCount  = firstLine.split(" ")(0).toInt
        val capacity    = firstLine.split(" ")(1).toInt

        // Fill array of items
        val items  = new Array[(Int, Int, Double, Int)](itemsCount)
        var value, weight, index: Int = 0; // Declare here for reuse in loop.
        for (line <- lines) {
            value  = line.split(" ")(0).toInt
            weight = line.split(" ")(1).toInt
            items(index) = (value, weight, value/weight.toDouble, index)
            index +=1
        }

        // Check for trivial
        if (capacity > items.map(x => x._2).sum) {
            println(items.map(x => x._2).sum)
            println(Array.fill(itemsCount){1}.mkString(" "))
            return
        }

        // Sort items by decreasing ratio, decreasing weights
        val sortedItems = items.sortBy(r => (-r._3, r._2))

        val Problem = new KnapsackBranchAndBound(sortedItems, capacity)
        val initialCandidate = Problem.root()

        // Priority Queue of tuples (optimistic_heuristic, partial_value, candidate)
        def ordering(t3: (Double,Int,Array[Int])) = (t3._1, t3._2)
        val queue = new PriorityQueue[(Double,Int,Array[Int])]()(Ordering.by(ordering))

        // Start loop
        var bestCandidate = Problem.root()
        queue.enqueue(bestCandidate)
        var optiHeuristic = 0.0;
        var partialValue = 0;

        while (queue.maxBy(ordering)._1 > bestCandidate._2) {
            for (child <- Problem.children(queue.dequeue)) {
                if (!Problem.exceedCapacity(child._3)) {
                    if (child._2 > bestCandidate._2)
                        bestCandidate = child
                    queue.enqueue(child)
                }
            }
        }

        println(bestCandidate._2)
        Problem.output(bestCandidate._3)
    }
}
