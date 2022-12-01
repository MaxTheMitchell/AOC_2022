import scala.io.Source
import util.chaining.scalaUtilChainingOps

@main def main: Unit = 
  var input = getInput
  println("Silver")
  silver(input)
  println("Gold")
  gold(input)

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

def getInput: String =
  Source.fromFile("./src/main/input/day1.txt").mkString