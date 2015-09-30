import scala.io.Source

class Knapsack(val items: Array[(Int,Int)], val capacity: Int) {
    println(s"Knapsack intialized with capacity $capacity")
}

object App {
    def main(args: Array[String]) {
        val filePath    = args(0)
        val source      = Source.fromFile(filePath)
        val lines       = source.getLines

        val firstLine   = lines.next()
        val items_count = firstLine.split(" ")(0).toInt
        val capacity    = firstLine.split(" ")(1).toInt
        val items       = new Array[(Int, Int, Double)](items_count)

        println(s"KnapSack of $items_count items and capacity $capacity")

        var value  = 0
        var weight = 0

        var index  = 0
        for (line <- lines){
            value  = line.split(" ")(0).toInt
            weight = line.split(" ")(1).toInt
            items(index) = (value, weight, value/weight.toDouble)
            index = index+1
        }

        val sortedItems = items.sortWith(_._3 > _._3)

        for (item <- sortedItems)
            println(item)
    }
}
