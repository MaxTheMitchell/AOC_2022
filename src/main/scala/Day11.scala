import util.chaining.scalaUtilChainingOps

object Day11 extends Day {
    def dayNum = 11

    def silver(input: String): Unit =
        parse(input)
            .pipe(addWorryMgmt((i) => (i / 3), _))
            .pipe(rounds(20, _))
            .values
            .map(_.itemsThrown)
            .toArray
            .sorted
            .takeRight(2)
            .product
            .pipe(println)

    def gold(input: String): Unit =
        val monks = parse(input)
        val worryMgmt = (i: BigInt) => (i % monks.values.map(_.div).product)
        addWorryMgmt(worryMgmt, monks)
            .pipe(rounds(10000, _))
            .values
            .map(_.itemsThrown)
            .toArray
            .sorted
            .takeRight(2)
            .product
            .pipe(println)
    
    def addWorryMgmt(mgmt: (BigInt) => BigInt, monkeys: Map[Int, Monkey]): Map[Int, Monkey] =
        monkeys.map((i, monk) => (i, monk.addWorryMgmt(mgmt))).toMap
    
    def rounds(n: Int, monkeys: Map[Int, Monkey]) =
        Range(0, n).foldLeft(monkeys)((monks, _) => round(monks))

    def round(monkeys: Map[Int, Monkey]): Map[Int, Monkey] =
        monkeys.keys.toArray.sorted.foldLeft(monkeys)((monks, i) => {
            val monk = monks(i)
            catchItems(monks, monk.throwItems) + (i -> monk.removeItems)
        })

    def catchItems(monkeys: Map[Int, Monkey], items: Array[(Int, BigInt)]): Map[Int, Monkey] =
        items.foldLeft(monkeys)((monks, tup) => 
            monks + ((tup.head, monks(tup.head).addItem(tup.last))) )

    class Monkey(val items: Array[BigInt], val opp: (BigInt) => BigInt, val div: Int, val trueMonk: Int, val falseMonk: Int, val itemsThrown: BigInt){
        def throwItems: Array[(Int, BigInt)] = 
            items.map(throwItem)

        def removeItems: Monkey =
            Monkey(Array(), opp, div, trueMonk, falseMonk, itemsThrown + items.length)

        def addItem(item: BigInt): Monkey =
            Monkey(items ++ Array(item), opp, div, trueMonk, falseMonk, itemsThrown)
        
        def addWorryMgmt(mgmt: (BigInt) => BigInt): Monkey = 
            Monkey(items, (i) => mgmt(opp(i)), div, trueMonk, falseMonk, itemsThrown)

        private def throwItem(item: BigInt): (Int, BigInt) =
            val worry = opp(item)
            (if worry % div == 0 then trueMonk else falseMonk, worry)
    }

    def parse(input: String): Map[Int, Monkey] = 
        input.split("\n\n").zipWithIndex.map((input, i) => (i, parseMonkey(input))).toMap

    def parseMonkey(input: String): Monkey =
        val arr = input.split("\n").tail
        val items = arr.head.split("  Starting items: ").last.split(", ").map((s) => BigInt(s.toInt))
        val opArr = arr(1).split("  Operation: new = ").last.split(" ")
        val opParser = (str: String) => str match {
            case "old" => (i: BigInt) => i
            case numStr => (_: BigInt) => BigInt(numStr.toInt)
        }
        val opperator: (BigInt, BigInt) => BigInt = opArr(1) match {
            case "+" => (_ + _)
            case "*" => (_ * _)
        }
        val opp = (old: BigInt) => opperator(opParser(opArr.head)(old), opParser(opArr.last)(old))
        val div = arr(2).split("  Test: divisible by ").last.toInt
        val trueMonk = arr(3).split("    If true: throw to monkey ").last.toInt
        val falseMonk = arr(4).split("    If false: throw to monkey ").last.toInt

        Monkey(items, opp, div, trueMonk, falseMonk, 0)
}
