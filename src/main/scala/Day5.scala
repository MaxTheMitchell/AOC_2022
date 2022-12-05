import util.chaining.scalaUtilChainingOps

object Day5 extends Day {
    def dayNum = 5

    def silver(input: String): Unit = 
        println(findSolution(input, (_.reverse)))

    def gold(input: String): Unit = 
        println(findSolution(input, ((a) => a)))

    def findSolution(input: String, order: Array[Char] => Array[Char]): String =
        parseInput(input)
            .pipe(doMoves(_, _, order))
            .toArray
            .map((i, lst) => (i, lst.head))
            .sorted
            .map(_.last)
            .mkString("")

    def doMoves(crates: Map[Int, Array[Char]], moves: Array[(Int, Int, Int)], order: Array[Char] => Array[Char]): Map[Int, Array[Char]] = 
        if moves.length == 0 then crates 
        else doMoves(doMove(crates, moves.head, order), moves.tail, order)

    def doMove(crates: Map[Int, Array[Char]], move: (Int, Int, Int), order: Array[Char] => Array[Char]): Map[Int, Array[Char]] =
        val (amt, from, to) = move
        val fromStack = crates.get(from).get
        val toStack = crates.get(to).get
        crates ++ Map(
            from -> fromStack.drop(amt).toArray,
            to -> fromStack.take(amt).toArray.pipe(order).concat(toStack)
        )

    def parseInput(input: String): 
        (Map[Int, Array[Char]], Array[(Int, Int, Int)]) = 
            val arr = input.split("\n\n")
            (parseCrates(arr.head), parseMoves(arr.last))

    def parseMoves(input: String): Array[(Int, Int, Int)] =
        input
            .split("\n")
            .map((line) => "\\D+".r.split(line).tail.map(_.toInt))
            .map { case Array(a, b, c) => (a, b, c) }

    def parseCrates(input: String): Map[Int, Array[Char]] = 
        input
            .split("\n")
            .dropRight(1)
            .flatMap((line) =>
                line
                    .zipWithIndex
                    .filter((c, i) => (i - 1) % 4 == 0 && c != ' ')
                    .map((c, i) => (((i - 1) / 4) + 1, c)))
            .groupMap(_.head)(_.last)
}