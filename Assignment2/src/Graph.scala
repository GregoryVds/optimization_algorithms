import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class Vertex(val index: Int, val xPos: Double, val yPos: Double, val incidenceList: ArrayBuffer[Edge]) {
  override def toString(): String = "Vertex: " + index + " at pos(" + xPos + "," + yPos + ")"
   
  def adjacentVertices() : ArrayBuffer[(Vertex, Int, Int)] = {
    incidenceList.map { edge => (edge.vertexB, edge.length, edge.consumption) }
  }
}
  
class Edge(val vertexA: Vertex, val vertexB: Vertex, val length: Int, val consumption: Int)

class Graph() {
  val verticesList = new HashMap[Int, Vertex]
     
  def addVertex(index: Int, xPos: Double, yPos: Double) : Unit = {
    verticesList.put(index, new Vertex(index, xPos, yPos, new ArrayBuffer()))
  }
    
  def addEdge(nodeA: Int, nodeB: Int, length: Int, capacity: Int) : Unit = {
    val A : Vertex = verticesList.get(nodeA).get
    val B : Vertex = verticesList.get(nodeB).get
    A.incidenceList.append(new Edge(A, B, length, capacity))
    B.incidenceList.append(new Edge(B, A, length, capacity))
  }
  
  def getVertex(index: Int) : Vertex = verticesList.get(index).get
}