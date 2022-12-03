import util.chaining.scalaUtilChainingOps

object Day2 extends Day {
    def dayNum: Int = 2

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
            val elfMove = elfMoves(a)
            (elfMove, myTatics(b)(elfMove))
        })

    def inputToMoves(input: Array[(Char, Char)]): Array[(Move, Move)] =
        input.map((a, b) => (elfMoves(a), myMoves(b)))

    def parseInput(input: String): Array[(Char, Char)] =
        input.split("\n").map(s => (s.charAt(0), s.charAt(2)))

}