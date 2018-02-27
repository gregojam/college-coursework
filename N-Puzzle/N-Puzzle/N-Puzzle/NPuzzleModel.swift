// N-Puzzle
// James Gregory    gregojam
// "Programming Assignment 105"
// "A290/A590 / Fall 2017"
// 10-9-17

// NPuzzleModel.swift

import Foundation

class NPuzzleModel {
    
    var board : [[Int]]
    var blank : [Int]
    
    init() {
        board = [[1, 2, 3],
                 [4, 5, 6],
                 [7, 8, 0]]
        
        blank = [2, 2]
    }
    
    /* Returns a list of all legal moves from current board state */
    func getLegalMoves() -> [Character] {
        var legalMoves : [Character] = []
        
        if blank[0] < 2 {
            legalMoves.append("U")
        }
        
        if blank[0] > 0 {
            legalMoves.append("D")
        }
        
        if blank[1] < 2 {
            legalMoves.append("L")
        }
        
        if blank[1] > 0 {
            legalMoves.append("R")
        }
        
        return legalMoves
    }
    
    /* Make a given Move */
    func makeMove(move : Character) {
        var moving : [Int] = blank
        switch move {
        case "U" :
            moving = [blank[0]+1, blank[1]]
            break
            
        case "D" :
            moving = [blank[0]-1, blank[1]]
            break
            
        case "L" :
            moving = [blank[0], blank[1]+1]
            break
            
        case "R" :
            moving = [blank[0], blank[1]-1]
            break
            
        default :
            moving = blank
        }
        
        board[blank[0]][blank[1]] = board[moving[0]][moving[1]]
        board[moving[0]][moving[1]] = 0
        blank = moving
    }
    
    /* Shuffle the tiles of the board in a manner that is guaranteed to be solvable */
    func shuffle() {
        var legalMoves : [Character]
        for _ in 0...999 {
            legalMoves = getLegalMoves()
            let index = Int(arc4random_uniform(UInt32(legalMoves.count)))
            
            makeMove(move: legalMoves[index])
        }
    }
    
    /* Determine if game is in a winning state */
    func isWon() -> Bool {
        for i in 0...8 {
            if board[i/3][i%3] != (i + 1) % 9 {
                return false
            }
        }
        return true
    }
}
