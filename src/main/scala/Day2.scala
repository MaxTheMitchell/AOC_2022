
import scala.io.Source
import util.chaining.scalaUtilChainingOps

object Day2 {

    trait Move {
        def value: Int
        def winTo: Move 
        def loseTo: Move
        def score(other :Move): Int = 
            value + 
            (if other == winTo then 6
            else if other == loseTo then 0
            else 3)
    }

    object Rock extends Move {
        def value = 1
        def loseTo = Paper
        def winTo = Scissors
    }

    object Paper extends Move {
        def value = 2
        def loseTo = Scissors
        def winTo = Rock
    }

    object Scissors extends Move {
        def value = 3
        def loseTo = Rock
        def winTo = Paper
    }

    @main def main: Unit = 
        var input = getInput
        println("Silver")
        silver(input)
        println("Gold")
        gold(input)

    def silver(input: String): Unit = 
        parseInput(input)
            .pipe(inputToMoves)
            .map((a, b) => b.score(a))
            .sum
            .pipe(println)

    def gold(input: String): Unit =
        parseInput(input)
            .pipe(inputToMovesGold)
            .map((a, b) => b.score(a))
            .sum
            .pipe(println)

    def elfMoves = 
        Map(
            'A' -> Rock,
            'B' -> Paper,
            'C' -> Scissors
        )

    def myMoves = 
        Map(
            'X' -> Rock,
            'Y' -> Paper,
            'Z' -> Scissors
        )

    def myTatics: Map[Char, (Move) => Move] = 
        Map(
            'X' -> (_.winTo),
            'Y' -> ((m) => m),
            'Z' -> (_.loseTo)
        )

    def inputToMovesGold(input: Array[(Char, Char)]): Array[(Move, Move)] =
        input.map((a, b) => {
            var elfMove = elfMoves(a)
            (elfMove, myTatics(b)(elfMove))
        })

    def inputToMoves(input: Array[(Char, Char)]): Array[(Move, Move)] =
        input.map((a, b) => (elfMoves(a), myMoves(b)))

    def parseInput(input: String): Array[(Char, Char)] =
        input.split("\n").map(s => (s.charAt(0), s.charAt(2)))

    def getInput: String =
        Source.fromFile("./src/main/input/day2.txt").mkString
}