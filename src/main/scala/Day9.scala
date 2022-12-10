import util.chaining.scalaUtilChainingOps

object Day9 extends Day {
    def dayNum = 9

    def silver(input: String): Unit =
        parse(input)
            .pipe(Knot(1).doMoves)
            .tailVisted
            .size
            .pipe(println)

    def gold(input: String): Unit =
        parse(input)
            .pipe(Knot(9).doMoves)
            .tailVisted
            .size
            .pipe(println)


    class Knot(private val x: Int, private val y: Int, private val child: Option[Knot], private val visted: Set[(Int, Int)]) {    
        def this(x: Int, y: Int, child: Option[Knot]) = this(x, y, child, Set()) 
        
        def this(n: Int) = 
             this(0, 0, if n == 0 then None else Some(Knot(n - 1))) 
         
        def tailVisted: Set[(Int, Int)] = 
            child match {
                case Some(c) => c.tailVisted
                case None => visted
            }
        
        def doMoves(moves: Array[Char]): Knot =
            moves.foldLeft(this)(_.move(_))

        def move(dir: Char): Knot =
            (dir match {
                case 'U' => Knot(x, y+1, child)
                case 'D' => Knot(x, y-1, child)
                case 'L' => Knot(x-1, y, child)
                case 'R' => Knot(x+1, y, child)
            }).moveChildren

        private def moveFromParent(px: Int, py: Int): Knot =
            val isAppart = (h: Int, t: Int) => (h - t).abs > 1 
            val changePos = (h: Int, t: Int) =>
                if h == t then t
                else if h > t then t + 1 
                else t - 1
            (if isAppart(px, x) || isAppart(py, y) 
                then Knot(changePos(px, x), changePos(py, y), child, visted) 
                else this).moveChildren
    
        private def moveChildren: Knot =
            child match {
                case Some(c) => Knot(x, y, Some(c.moveFromParent(x, y)))
                case None => Knot(x, y, None, visted + ((x,y)))
            }

    }

    def parse(input: String): Array[Char] =
        input.split("\n").flatMap(str => 
            Range(0, str.drop(2).toInt).map((_) => str.head))
}
