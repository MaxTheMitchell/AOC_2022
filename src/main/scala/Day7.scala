import util.chaining.scalaUtilChainingOps

object Day7 extends Day {
    def dayNum = 7

    def silver(input: String): Unit = 
        parse(input)
            .pipe(runCmds)
            .pipe(_.sumUnder100000)
            .pipe(println)

    def gold(input: String): Unit = 
        val dir = parse(input).pipe(runCmds)
        val mustDelete = 30000000 - (70000000 - dir.totalSize)
        println(dir.minDelete(mustDelete))

    class Dir(filesSize: Int, dirs: Map[String, Dir]){
        def getDir(key: String): Dir = dirs(key)

        def setDir(key: String, dir: Dir): Dir = 
            Dir(filesSize, dirs + (key -> dir))

        def addSize(size: Int): Dir =
            Dir(filesSize + size, dirs)

        def totalSize: Int = 
            filesSize + dirs.values.map(_.totalSize).sum

        def sumUnder100000: Int = 
            dirs.values.map(_.sumUnder100000).sum + (if totalSize <= 100000 then totalSize else 0)
    
        def minDelete(target: Int): Int = 
            dirs
                .values
                .map(_.minDelete(target))
                .filter(_ >= target)
                .foldRight(totalSize)(_.min(_))
    }   

    def runCmds(cmds: Array[(String, String)]): Dir = 
        runFromRoot(Dir(0, Map()), cmds).head    

    def runFromRoot(root: Dir, cmds: Array[(String, String)]): (Dir, Array[(String, String)]) =
        val (newRoot, newCmds, _) = runDirCmds(root, cmds)
        if newCmds.length == 0 then (newRoot, newCmds)
        else runFromRoot(newRoot, newCmds) 
    
    def runDirCmds(dir: Dir, cmds: Array[(String, String)]): (Dir, Array[(String, String)], Boolean) =
        if cmds.length == 0 then (dir, cmds, true) 
        else {
            val (cmd, arg) = cmds.head
            cmd match {
                case "cd" => arg.dropRight(1) match {
                    case "/" => (dir, cmds.tail, true)
                    case ".." => (dir, cmds.tail, false)
                    case dirName => {
                        val (childDir, newCmds, goHome) = runDirCmds(dir.getDir(dirName), cmds.tail)
                        val newDir = dir.setDir(dirName, childDir)
                        if goHome then (newDir, newCmds, true)
                        else runDirCmds(newDir, newCmds)
                    }
                }
                case "ls" => runDirCmds(parseDir(arg), cmds.tail)
            }
        }

    def parseDir(input: String): Dir = 
        input.split("\n").foldRight(Dir(0, Map()))((str, dir) => {
            val arr = str.split(" ")
            arr.head match {
                case "dir" => dir.setDir(arr.last, Dir(0, Map()))
                case size => dir.addSize(size.toInt)
            }
        })
    
    def parse(input: String): Array[(String, String)] =
        input.split("\\$ ").tail.map((s) =>{
            (s.take(2), s.drop(3))
        })
} 
