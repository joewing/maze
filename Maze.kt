// Maze generator in Kotlin
// Joe Wingbermuehle
// 2015-07-25

class Maze(val width: Int, val height: Int) {

    private enum class Cell {
        WALL, SPACE
    }

    private val data = Array(width, {i ->
        Array<Cell>(height, {i -> Cell.WALL})
    })
    private val rand = java.util.Random()

    init {
        generate()
    }

    private fun carve(x: Int, y: Int) {

        val upx = intArrayOf(1, -1, 0, 0)
        val upy = intArrayOf(0, 0, 1, -1)

        var dir = rand.nextInt(4)
        var count = 0
        while(count < 4) {
            val x1 = x + upx[dir]
            val y1 = y + upy[dir]
            val x2 = x1 + upx[dir]
            val y2 = y1 + upy[dir]
            if(data[x1][y1] == Cell.WALL && data[x2][y2] == Cell.WALL) {
                data[x1][y1] = Cell.SPACE
                data[x2][y2] = Cell.SPACE
                carve(x2, y2)
            } else {
                dir = (dir + 1) % 4
                count += 1
            }
        }

    }

    fun generate() {

        for(x in 0..width - 1) {
            for(y in 0..height - 1) {
                data[x][y] = Cell.WALL
            }
        }

        for(x in 0..width - 1) {
            data[x][0] = Cell.SPACE
            data[x][height - 1] = Cell.SPACE
        }
        for(y in 0..height - 1) {
            data[0][y] = Cell.SPACE
            data[width - 1][y] = Cell.SPACE
        }

        data[2][2] = Cell.SPACE
        carve(2, 2)

        data[2][1] = Cell.SPACE
        data[width - 3][height - 2] = Cell.SPACE
    } 

    fun print() {
        for(y in 0..height - 1) {
            for(x in 0..width - 1) {
                if(data[x][y] == Cell.WALL) {
                    print("[]")
                } else {
                    print("  ")
                }
            }
            println()
        }
    }

}

fun main(args: Array<String>) {
    val m = Maze(39, 23)
    m.generate()
    m.print()
}
