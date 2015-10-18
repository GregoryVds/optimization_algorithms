import scala.collection.mutable.ArrayBuffer

class State(val vertex: Vertex, val length: Int, val consumption: Int, val heuristic: Int, val path: Array[Int])

class ShortestPathBranchAndBound(source: Int, dest:Int, maxCapacity:Int, graph:Graph) {  
  
  def root() : State = new State(graph.getVertex(source), 0, 0, 9999, Array(source))
    
  def children(state: State): Seq[State] =
    state.vertex.adjacentVertices.map { ajd => {
        new State(ajd._1, state.length+ajd._2, state.consumption+ajd._3, 9999, state.path:+ajd._1.index)
      }
    }
 
  def goalTest(state: State): Boolean = state.path.last == dest
  
  def exceedCapacity(state: State): Boolean = state.consumption > maxCapacity
  
  def output(state: State): Unit = {
    println("Length: "+state.length)
    println("Consumption: "+state.consumption)
    println(state.path.mkString("-"))
  }
}  