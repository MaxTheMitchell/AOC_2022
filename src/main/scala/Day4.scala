import util.chaining.scalaUtilChainingOps

object Day4 extends Day {
    def dayNum: Int = 4

    def silver(input: String): Unit = 
        parseInput(input)
            .count(fulloverlapEither)
            .pipe(println)

    def gold(input: String): Unit = 
        parseInput(input)
            .count(partialoverlapEither)
            .pipe(println)

    def partialoverlapEither(a: (Int, Int), b: (Int, Int)): Boolean =
        partialOverLap.tupled(a ++ b) || partialOverLap.tupled(b ++ a) 

    def partialOverLap(aStart: Int, aEnd: Int, bStart: Int, bEnd: Int): Boolean =
        (aStart <= bStart && aEnd >= bStart) || (aStart <= bEnd && aEnd >= bEnd) 

    def fulloverlapEither(a: (Int, Int), b: (Int, Int)): Boolean =
        fulloverlap.tupled(a ++ b) || fulloverlap.tupled(b ++ a) 

    def fulloverlap(aStart: Int, aEnd: Int, bStart: Int, bEnd: Int): Boolean =
        aStart <= bStart && aEnd >= bEnd

    def parseInput(input: String): Array[((Int, Int), (Int, Int))] =
        input.split("\n").map((line) => { 
            val ranges = line.split(",").map((str) => {
                val arr = str.split("-").map(_.toInt)
                (arr.head, arr.last)
            })
            (ranges.head, ranges.last)
        })
}
