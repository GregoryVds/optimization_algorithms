import scala.io.Source
import scala.collection.mutable.PriorityQueue
import scala.util.control.Breaks._

class KnapsackBranchAndBound(items: Array[(Int,Int,Double,Int)], capacity: Int) {
    val value   = items.map(x => x._1)
    val weight  = items.map(x => x._2)
    val ratio   = items.map(x => x._3)
    val index   = items.map(x => x._4)

    val n = items.length
    var bestSolution = 0

    def partialWeight(candidate: Array[Int]) =
        (0 until candidate.length).map(i => candidate(i)*weight(i)).sum

    def candidateValue(candidate: Array[Int]) =
        (0 until candidate.length).map(i => candidate(i)*value(i)).sum

    def root() = Array[Int]()

    def reject(candidate: Array[Int]) =
        partialWeight(candidate) > capacity

    def accept(candidate: Array[Int]) =
        candidate.length == items.length && candidateValue(candidate) > bestSolution

    def children(candidate: Array[Int]) : Seq[Array[Int]] =
        if (candidate.length == n) Seq()
        else Seq(candidate:+1, candidate:+0)

    def output(candidate: Array[Int]) {
        var solution = Array.fill(n){0}
        for (i <- 0 until candidate.length) {
            solution(index(i)) = candidate(i)
        }
        println(solution.mkString(" "))
    }

    // def checkTrivialSolution(candidate: Array[Int])
    //     true

    def optimisticHeuristic(candidate: Array[Int]) : Double = {
        // printf("Computing optimistic heuristic for %s \n", candidate.mkString(" "))
        var currentValue : Double  = candidateValue(candidate)
        var currentWeight : Double = partialWeight(candidate)
        //// println(s"Current_value:$currentValue and current_weight: $currentWeight")

        breakable {
            for (i <- candidate.length until items.length) {
                if ((weight(i) + currentWeight) < capacity) {
                    currentWeight += weight(i)
                    currentValue  += value(i)
                } else {
                    val capacityLeft = (capacity - currentWeight)
                    val fractionOfLastItem = capacityLeft/weight(i)
                    // printf("Fraction %f of item %d\n", fractionOfLastItem, (i+1))
                    currentValue += fractionOfLastItem*value(i)
                    break
                }
            }
        }
        //println(s"Total value of: $currentValue")
        currentValue
    }

    def partialKnapsackValue(candidate: Array[Int]) {
        var currentWeight = partialWeight(candidate)
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
        var value  = 0 // Declare here for reuse in loop.
        var weight = 0
        var index  = 0
        for (line <- lines){
            value  = line.split(" ")(0).toInt
            weight = line.split(" ")(1).toInt
            items(index) = (value, weight, value/weight.toDouble, index)
            index = index+1
        }

        val sortedItems = items.sortBy(r => (-r._3, r._2))


        val Problem = new KnapsackBranchAndBound(sortedItems, capacity)
        val initialCandidate = Problem.root()

        def ordering(t2: (Double,Array[Int])) = t2._1
        val queue = new PriorityQueue[(Double,Array[Int])]()(Ordering.by(ordering))

        var bestValue      = 0
        var bestValueCompo = Array.empty[Int]

        val rootNode = (Problem.optimisticHeuristic(initialCandidate), initialCandidate)
        queue.enqueue(rootNode)

        // Start loop
        while (queue.maxBy(ordering)._1 > bestValue) {
            for (child <- Problem.children(queue.dequeue._2)) {

                if (!Problem.reject(child)) {
                    var childValue      = Problem.candidateValue(child)
                    val optiHeuristic   = Problem.optimisticHeuristic(child)
                    if (childValue > bestValue) {
                        bestValue       = childValue
                        bestValueCompo  = child
                    }
                    queue.enqueue((optiHeuristic, child))
                    //println("Enqueud %f", optiHeuristic)
                }
            }
            //println("Queue size:" + queue.size)
        }

        println(bestValue)
        Problem.output(bestValueCompo)

        // println("Solution size:"+bestValueCompo.size)
        // println("Solution load:"+Problem.partialWeight(bestValueCompo))
        // println("Solution value:"+Problem.candidateValue(bestValueCompo))


    }
}
