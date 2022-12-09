import util.chaining.scalaUtilChainingOps

object Day8 extends Day {
    def dayNum = 8 

    def silver(input: String): Unit =
        parse(input)
            .pipe(nSeeable)
            .pipe(println)

    def gold(input: String): Unit =
        parse(input)
            .pipe(mostSenic)
            .pipe(println)

    def mostSenic(grid: Array[Array[Int]]): Int = 
        indexs(grid).map(senic(_, _, grid)).reduce(_.max(_))
    
    def senic(x: Int, y: Int, grid: Array[Array[Int]]): Int =
        senicRow(x, grid(y)) * senicRow(y, col(x, grid))
    
    def nSeeable(grid: Array[Array[Int]]): Int = 
        indexs(grid).count((x, y) => canSee(x, y, grid))

    def canSee(x: Int, y: Int, grid: Array[Array[Int]]): Boolean =
        seeRow(x, grid(y)) || seeRow(y, col(x, grid))


    def indexs(grid: Array[Array[Int]]): Array[(Int, Int)] = 
        Range(0, grid.length).flatMap((x) => 
            Range(0, grid(x).length).map((y) => (x, y))).toArray


    def col(y: Int, grid: Array[Array[Int]]): Array[Int] =
        grid.map((row) => row(y))

    def seeRow(i: Int, row: Array[Int]): Boolean =
        val isGood = (arr: Array[Int]) => arr.forall(_  < row(i))
        isGood(row.take(i)) || isGood(row.drop(i + 1))

    def senicRow(i: Int, row: Array[Int]): Int =
        val howSenic = (arr: Array[Int]) => 
            (arr.takeWhile(_ < row(i)).length + 1).min(arr.length)
        howSenic(row.take(i).reverse) * howSenic(row.drop(i + 1))

    def parse(input: String): Array[Array[Int]] =
        input.split("\n").map(_.split("").map(_.toInt))
}
