import scala.io.Source

abstract class Day {
    def runProd = run("input")
    def runTest = run("test")

    def dayNum: Int
    def silver(input: String): Unit
    def gold(input: String): Unit

    private def run(inputType: String): Unit = 
        val input = Source.fromFile("./src/main/%s/day%d.txt".format(inputType, dayNum)).mkString
        println("Silver")
        silver(input)
        println("Gold")
        gold(input)
}
