import util.chaining.scalaUtilChainingOps

object Day3 extends Day {
    def dayNum: Int = 3

    def silver(input: String): Unit = 
        parseInput(input)
            .map(components)
            .map(overLapChar)
            .map(charValue)
            .sum
            .pipe(println)

    def gold(input: String): Unit = 
        parseInput(input)
            .pipe(elfGroups)
            .map(elfGroupOverlap)
            .map(charValue)
            .sum
            .pipe(println)

    def elfGroupOverlap(group: Array[String]): Char =
        group.map(_.toSet).reduce(_ & _).head

    def elfGroups(input: Array[String]): Array[Array[String]] =
        input.zipWithIndex
            .groupBy(_.last / 3)
            .values
            .map(_.map(_.head))
            .toArray

    def charValue(c: Char): Int = 
        c.toInt - (if c.isUpper then 'A'.toInt - 27 else 'a'.toInt - 1)

    def overLapChar(comp1: String, comp2: String): Char =
        (comp1.toSet & comp2.toSet).head

    def components(str: String): (String, String) = str.splitAt(str.length / 2)

    def parseInput: String => Array[String] = _.split("\n")
}
