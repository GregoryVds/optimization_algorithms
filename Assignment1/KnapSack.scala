import scala.io.Source

object KnapSack {
    def main(args: Array[String]) {
        val filePath    = args(0)
        val source      = Source.fromFile(filePath)
        val lines       = source.getLines

        val firstLine = lines.next()
        val ITEMS_COUNT = firstLine.split(" ")(0).toInt
        val CAPACITY    = firstLine.split(" ")(1).toInt

        println(s"KnapSack of $ITEMS_COUNT items and capacity $CAPACITY")

        for (line <- lines)
            println(line)
    }
}
