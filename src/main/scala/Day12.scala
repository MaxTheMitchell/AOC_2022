import util.chaining.scalaUtilChainingOps

object Day12 extends Day {
    def dayNum = 12

    def silver(input: String): Unit =
        parse(input)
            .pipe((start) => dijkstra(Map(start -> 0)))
            .find((node, _) => node.isEnd)
            .get
            .last
            .pipe(println)


    def gold(input: String): Unit =
         parse(input)
            .pipe((start) => dijkstra(Map(start -> 0)))
            .keys
            .filter(_.elivation == 'a'.toInt)
            .map((aStart) => dijkstra(Map(aStart -> 0)).find((node, _) => node.isEnd))
            .filter(_.isDefined)
            .map(_.get.last)
            .min
            .pipe(println)

    def dijkstra(used: Map[Node, Int]): Map[Node, Int] = 
        val unusedVerts = used.toArray.flatMap((node, i) => node.neibors.filter((n) => !used.contains(n)).map((node) => (node, i + 1)))
        if unusedVerts.length == 0 then used
        else {
            val (node, dist) = unusedVerts.minBy(_.last)
            val newMap = node.neibors.filter((n) => used.contains(n) && used(n) > dist + 1).map((n) => (n, dist + 1)).toMap
            dijkstra(used ++ newMap + (node -> dist))
        }

    def parse(input: String): Node = 
        val grid = input.split("\n").map(_.map((c) => 
            c match {
                case 'S' => Node('a'.toInt, Array(), false, true)
                case 'E' => Node('z'.toInt, Array(), true, false)
                case c   => Node(c.toInt)
            }).toArray)
        addNeibors(grid)

    def addNeibors(nodes: Array[Array[Node]]): Node =
        nodes.zipWithIndex.foreach((row, x) => row.zipWithIndex.foreach((node, y) => {
            node.neibors = Array((x - 1, y), (x + 1, y), (x,  y - 1), (x, y + 1))
                .map((x, y) => nodes.lift(x).map((row) => row.lift(y)))
                .filter(_.isDefined)
                .map(_.get)
                .filter(_.isDefined)
                .map(_.get)
                .filter((n) => node.elivation + 1 >= n.elivation)
        }))
        nodes.reduce(_ ++ _).find(_.isStart).get
    
    class Node(val elivation: Int, var neibors: Array[Node], val isEnd: Boolean, val isStart: Boolean){
        def this(elivation: Int) =
            this(elivation, Array(), false, false)
    }
}