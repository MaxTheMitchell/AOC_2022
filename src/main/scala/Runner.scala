import scala.io.Source

object Runner {
    
    def runProd = run("input", _, _, _)

    def runTest = run("test", _, _, _)

    private def run(inputType: String, day: Int, silver: (String) => Unit, gold: (String) => Unit): Unit = 
        val input = Source.fromFile("./src/main/%s/day%d.txt".format(inputType, day)).mkString
        println("Silver")
        silver(input)
        println("Gold")
        gold(input)
}
