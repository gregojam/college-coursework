//////////////////////////////
//
//C212 Spring 2016
//Homework 4
//
//Author: James Gregory
//Last Modified: 2/19/16
//
//////////////////////////////

public class MagicSquare{
  
  // constructor
  private MagicSquare(){}
  
  // returns a magic square
  public static int[][] makeMagicSquare(int N){
    if (N % 2 == 0 || N < 0){
      System.out.println("Invalid input");
      return null;
    }
    else{
      int mid = (N * N + 1) / 2;
      int row = 0;
      int col = N / 2;
      int[][] matrix= new int[N][N];
      for (int i = 1; i <= N * N; i++){
        matrix[row][col] = i;
        if (matrix[(row + mid) % N][(col + mid) % N] == 0){
          row = (row + mid) % N;
          col = (col + mid) % N;
        }
        else
          row = (++row) % N;
      }
      return matrix;
    }
  }
  
  // checks each number only occurs once and that matrix is square
  private static boolean checkEm(int[][] nums){
    if (nums.length == nums[0].length){
      for (int k = 1; k <= nums.length * nums.length; k++){
        int check = 1;
        for (int i = 0; i < nums.length; i++){
          for (int j = 0; j < nums.length; j++){
            if (k == nums[i][j])
              check--;
          }
        }
        if (check != 0)
          return false;
        else; // Move Along!
      }
      return true;
    }
    else
      return false;
  }
      

  // determines if an integer matrix is a normal magic square
  public static boolean isMagic(int[][] magic){
    if(checkEm(magic)){
      int correct = ((int)Math.pow(magic.length, 3) + magic.length) / 2;
      int row = 0, col = 0, diag1 = 0, diag2 = 0;
      for (int i = 0; i < magic.length; i++){
        row = 0; 
        col = 0;
        for (int j = 0; j < magic.length; j++){
          row += magic[i][j];
          col += magic[j][i];
          if (i == j)
            diag1 += magic[i][j];
          if (i + j == magic.length - 1)
            diag2 += magic[i][j];
        }
        if (row != correct || col != correct)
          return false;
      }
      if (diag1 != correct || diag2 != correct)
        return false;
      else
        return true;
    }
    else
      return false;
  }
  
  public static void main(String[] args){}
}