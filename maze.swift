// Maze generator in Swift
// 2014-12-11 - Joe Wingbermuehle
// 2018-02-20 - Sarp Başaraner - Update for Swift 4
// Build with:
//       xcrun -sdk macosx swift maze.swift 

import Foundation

class Maze {

    enum Cell {
        case Space, Wall
    }

    var data: [[Cell]] = []

    // Generate a random maze.
    init(width: Int, height: Int) {
        for _ in 0 ..< height {
            data.append([Cell](repeating: Cell.Wall, count: width))
        }
        for i in 0 ..< width {
            data[0][i] = Cell.Space
            data[height - 1][i] = Cell.Space
        }
        for i in 0 ..< height {
            data[i][0] = Cell.Space
            data[i][width - 1] = Cell.Space
        }
        data[2][2] = Cell.Space
        self.carve(x: 2, y: 2)
        data[1][2] = Cell.Space
        data[height - 2][width - 3] = Cell.Space
    }

    // Carve starting at x, y.
    func carve(x: Int, y: Int) {
        let upx = [1, -1, 0, 0]
        let upy = [0, 0, 1, -1]
        var dir = Int(arc4random_uniform(4))
        var count = 0
        while count < 4 {
            let x1 = x + upx[dir]
            let y1 = y + upy[dir]
            let x2 = x1 + upx[dir]
            let y2 = y1 + upy[dir]
            if data[y1][x1] == Cell.Wall && data[y2][x2] == Cell.Wall {
                data[y1][x1] = Cell.Space
                data[y2][x2] = Cell.Space
                carve(x: x2, y: y2)
            } else {
                dir = (dir + 1) % 4
                count += 1
            }
        }
    }

    // Show the maze.
    func show() {
        for row in data {
            for cell in row {
                if cell == Cell.Space {
                    print("  ", separator: "", terminator: "")
                } else {
                    print("[]", separator: "", terminator: "")
                }
            }
            print("")
        }
    }

}

let maze = Maze(width: 39, height: 23)
maze.show()
