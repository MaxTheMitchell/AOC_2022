object Day6 extends Day {
    def dayNum = 6

    def silver(input: String): Unit = 
        println(findStart(input, 4))

    def gold(input: String): Unit = 
        println(findStart(input, 14))


    def findStart(input: String, n: Int): Int = 
        if input.take(n).toSet.size == n
        then n
        else 1 + findStart(input.tail, n)
} 
