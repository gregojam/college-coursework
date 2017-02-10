/*************************************************************************
 * Author:       Earl Dean
 *
 * Compilation:  javac Solver.java
 * Execution:    java  Solver
 * Dependencies: Board.java
 *
 * Description:  Solves N-puzzle with A* Algorithm  
 *
 *************************************************************************/

import java.util.Scanner;
import java.io.File;
import java.io.FileNotFoundException;

public class Solver {
    
    private MinPQ<SearchNode> pq;
    private MinPQ<SearchNode> twinPq;
    private boolean solvable = true;
    private SearchNode goal = null;
    
    public Solver(Board initial) {
        pq = new MinPQ<>();
        pq.insert(new SearchNode(initial, 0, null));
        
        twinPq = new MinPQ<>();
        Board twin = initial.twin();
        twinPq.insert(new SearchNode(twin, 0, null));
        
        boolean solved = false;
        while (!solved) {
            solved = solve();
        }
    }
    
    private boolean solve() {
        // solve initial board 
        SearchNode node = pq.delMin();
        if (node.board.isGoal()) {
            goal = node;
            return true;
        }
        
        for (Board neighbor : node.board.neighbors()) {
            if (node.prev != null) {
                if (neighbor.equals(node.prev.board)) {
                    continue;
                }
            }
            pq.insert(new SearchNode(neighbor, node.moves+1, node));
        }
        
        // solve twin board 
        SearchNode twinNode = twinPq.delMin();
        if (twinNode.board.isGoal()) {
            solvable = false;
            return true;
        }
        for (Board neighbor : twinNode.board.neighbors()) {
            if (node.prev != null) {
                if (neighbor.equals(node.prev.board)) {
                    continue;
                }
            }
            twinPq.insert(new SearchNode(neighbor, twinNode.moves+1, node));
        }
        
        twinPq.insert(new SearchNode(node.board.twin(), 0, null));
        return false;
    }
    
    // is board solvable?
    public boolean isSolvable() {
        return solvable;
    }
    
    // min # of moves to solve board 
    // -1 if not solvable
    public int moves() {
        if (goal == null) {
            return -1;
        } else {
            return goal.moves;
        }
    }
    
    public Iterable<Board> solution() {
        Stack<Board> path = new Stack<>();
        SearchNode current = goal;
        while (current.prev != null) {
            path.push(current.board);
            current = current.prev;
        }
        return path;
    }
    
    // Wrapper class for Board in A* algorithm
    private class SearchNode implements Comparable<SearchNode> {
        private int moves; 
        private int priority;
        private Board board;
        private SearchNode prev;
        
        private SearchNode(Board b, int move, SearchNode prev) {
            moves = move;
            board = b;
            this.prev = prev;
            priority = board.manhattan() + moves;
        }
        
        public int compareTo(SearchNode s) {
            if (this.priority < s.priority) {
                return -1;
            } else {
                return 1;
            }
        }
    }
    
    // unit test 
    public static void testBoard(String f) {
        // create initial board from file
        File file = new File(f); 
        Scanner scan;
        
        // try to read the file 
        try {
            scan = new Scanner(file);
        } catch (FileNotFoundException e) { 
            e.printStackTrace();
            return;
        }
        
        int N = scan.nextInt();
        
        int[][] blocks = new int[N][N];
        for (int i = 0; i < N; i++)
            for (int j = 0; j < N; j++)
            blocks[i][j] = scan.nextInt();
        Board initial = new Board(blocks);
        
        // solve the puzzle
        Solver solver = new Solver(initial);
        // print solution to standard output
        if (!solver.isSolvable())
            System.out.println("No solution possible");
        else {
            System.out.println("Minimum number of moves = " + solver.moves());
            for (Board board : solver.solution())
                System.out.println(board);
        }
    }
    
    public static void main(String[] args) {
       System.out.println("--- Testing puzzle04 ---");
       testBoard("puzzle04.txt");
       System.out.println("");
       
       System.out.println("--- Testing puzzle3x3-unsolvable.txt ---");
       testBoard("puzzle3x3-unsolvable.txt");
       System.out.println("");
       
       System.out.println("--- Testing puzzle01 ---");
       testBoard("puzzle01.txt");
    }
}