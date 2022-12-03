import util.chaining.scalaUtilChainingOps

object Day1 extends Day {
    def dayNum: Int = 1

    def silver(input: String): Unit = 
    parseInput(input)
        .map(_.sum)
        .max
        .pipe(println)

    def gold(input: String): Unit =
    parseInput(input)
        .map(_.sum)
        .sorted
        .reverse
        .take(3)
        .sum
        .pipe(println)

    def parseInput(input: String): Array[Array[Int]] =
        input.split("\n\n").map(_.split("\n").map(_.toInt))

}