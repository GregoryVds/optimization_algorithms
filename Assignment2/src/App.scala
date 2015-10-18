import scala.io.Source
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.HashMap

object App {

  def dominated(state: State, closedList: Array[(Int, Int)]) : Boolean = {
    for (closed <- closedList) {
      if (closed._1 < state.length && closed._2 < state.consumption)
        return true
    }
    return false
  }
  
  def main(args: Array[String]): Unit = {
    // Program stops itself after X seconds in case an optimum answer is not found yet
    val SECONDS_ALLOWED = 285 // 4min45
    val startTime = System.nanoTime()
    val timeLimit = startTime+(1000*1000*1000*SECONDS_ALLOWED.toDouble)
    
    val lines            = Source.fromFile(args(0)).getLines
    
    // Extract first line parameters
    val firstLineParams  = lines.next().split(" ")
    val nodesCount       = firstLineParams(0).toInt
    val edgesCount       = firstLineParams(1).toInt
    val sourceNode       = firstLineParams(2).toInt
    val destNode         = firstLineParams(3).toInt
    val maxCapacity      = firstLineParams(4).toInt
    
    println("Compute Shortest Path under Ressource Constraints")
    printf("NODES_COUNT: %d\n", nodesCount)
    printf("EDGES_COUNT: %d\n", edgesCount)
    printf("SOURCE_NODE: %d\n", sourceNode)
    printf("DEST_NODE: %d\n",   destNode)
    printf("MAX_CAPACITY: %d\n",maxCapacity)
    
    // Build Problem Graph
    val graph = new Graph()
    for (i <- 0 until nodesCount) {
      val xCoord :: yCoord :: _ = lines.next.split(" ").map { x => x.toDouble }.toList
      graph.addVertex(i, xCoord, yCoord)
    }
 
    for (i <- 0 until edgesCount) {
      val source :: dest :: weight :: consumption :: _ = lines.next.split(" ").map { x => x.toInt }.toList
      graph.addEdge(source, dest, weight, consumption)
    }
  
    // Initialize A* search
    val problem = new ShortestPathBranchAndBound(sourceNode, destNode, maxCapacity, graph)
    
    // Priority Queue for choosing next node to expand
    def ordering(state: State) = (-state.heuristic, -state.consumption)
    val queue = new PriorityQueue[State]()(Ordering.by(ordering))
    
    // HashMap of closed states nodeIndex => (length, consumption)
    val closedList = new HashMap[Int, Array[(Int, Int)]]
    
    var bestCandidate = new State(graph.getVertex(0), Int.MaxValue, 0, 0, Array(0))
    queue.enqueue(problem.root())
    
    while (!queue.isEmpty && System.nanoTime() < timeLimit) {
      val bestState         = queue.dequeue
      val exploredNodesList = closedList.get(bestState.vertex.index)
     
      // Only consider expanding if not in explored list, or already in explored but not dominated.
      if (exploredNodesList == None || !dominated(bestState, exploredNodesList.get)) {
        
         // Expanding node, we loop among all children states.
         for (child <- problem.children(bestState)) {
           if (!problem.exceedCapacity(child)) {
             // Check if we have a better feasible solution
             if (problem.goalTest(child) && child.length < bestCandidate.length)
               bestCandidate = child
               
             queue.enqueue(child)
             // Check 
           }
         } // For
      }
    } // While
    
    problem.output(bestCandidate)
  }
}