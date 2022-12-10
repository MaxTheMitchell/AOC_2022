import util.chaining.scalaUtilChainingOps

object Day10 extends Day {
    def dayNum = 10

    def silver(input: String): Unit =
        parse(input)
            .pipe(run)
            .pipe((feqs) => 
                Array(20, 60, 100, 140, 180, 220).map((n) => 
                    feqs(n) * n
                ).sum)
            .pipe(println)

    def gold(input: String): Unit =
        parse(input)
            .pipe(run)
            .pipe(crtDisplay)
            .pipe(println)

    def crtDisplay(freqs: Array[Int]): String = 
        freqs.drop(2).zipWithIndex
            .map((freq, i) => {
                val pixI = (i % 40)
                (if (freq - (pixI + 1)).abs < 2 then "#" else ".") ++ ((if pixI == 39 then "\n" else ""))
            })
            .reduce(_ ++ _)

    def run(instructions: Array[(Int) => Int]): Array[Int] =
        Array(1) ++ instructions.scanLeft(1)((reg, inst) => inst(reg))

    def parse(input: String): Array[(Int) => Int] = 
        input.split("\n").flatMap((line) => {
            val arr = line.split(" ")
            arr.head match {
                case "noop" => Array((i: Int) => i)
                case "addx" => Array((i: Int) => i, (i: Int) => i + arr.last.toInt)
            }
        }) 
}