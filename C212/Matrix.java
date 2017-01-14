//////////////////////////////
//
//C212 Spring 2016
//Homework 4
//
//Author: James Gregory
//Last Modified: 2/19/16
//
//////////////////////////////

// ***NOTE***
// definitions for upper and lower diagonals of matrices
// is reversed in homework. Functions upperDiagonal() and
// lowerdiagonal() return appropriate triangular matrices
// as per their corrected definitions.

public class Matrix{
  
  public final double[][] matrix;
  public final int ROW;
  public final int COL;
  
  //constructor
  public Matrix(double[][] m){
    matrix = m;
    ROW = matrix.length;
    COL = matrix[0].length;
  }
  
  // returns matrix as a string
  public String toString(){
    String s = "";
    for(int i = 0; i < ROW; i++){
      s+= "[";
      for(int j = 0; j < COL; j++){
        s += matrix[i][j];
        if(j < matrix[0].length - 1)
          s+= ", ";
        else
          s+= "]\n";
      }
    }
    return s;
  }
  
  // returns the transpose of matrix
  public Matrix transpose(){
    Matrix m = new Matrix(new double[COL][ROW]);
    for(int i = 0; i < m.ROW; i++){
      for(int j = 0; j < m.COL; j++)
        m.matrix[i][j] = matrix[j][i];
    }
    return m;
  }
    
  // returns the upper triangular matrix of matrix
  public Matrix upperDiagonal(){
    Matrix m = new Matrix(new double[ROW][COL]);
    for(int i = 0; i < ROW; i++){
      for(int j = 0; j < COL; j++){
        if(i > j)
          m.matrix[i][j] = 0;
        else
          m.matrix[i][j] = matrix[i][j];
      }
    }
    return m;
  }

  // returns the lower triangular matrix of matrix
  public Matrix lowerDiagonal(){
    Matrix m = new Matrix(new double[ROW][COL]);
    for(int i = 0; i < ROW; i++){
      for(int j = 0; j < COL; j++){
        if(i < j)
          m.matrix[i][j] = 0;
        else
          m.matrix[i][j] = matrix[i][j];
      }
    }
    return m;
  }
  
  // returns the diagonal of matrix
  public Matrix diagonalMatrix(){
    Matrix m = new Matrix(new double[ROW][COL]);
    for(int i = 0; i < ROW; i++){
      for(int j = 0; j < COL; j++){
        if(i != j)
          m.matrix[i][j] = 0;
        else
          m.matrix[i][j] = matrix[i][j];
      }
    }
    return m;
  }
  
  // multiplies two matrices together. If impossible, returns null
  public Matrix multiply(Matrix other){
    if (COL == other.ROW){
      Matrix m = new Matrix(new double[ROW][other.COL]);
      for (int i = 0; i < m.ROW; i++){
        for (int j = 0; j < m.COL; j++){
          for (int k = 0; k < COL; k++)
            m.matrix[i][j] += matrix[i][k] * other.matrix[k][j];
        }
      }
      return m;
    }
    else
      return null;
  }
  
  public static void main(String[] args){
    double[][] m = {{5, 4, 3}, {4, 0, 4}, {7, 10, 3}};
    Matrix a = new Matrix(m);
    
    double[][] n = {{5, 4}, {4, 0}, {7, 10}, {-1, 8}};
    Matrix b = new Matrix(n);
    
    System.out.println("\nTest Block" + "\n====================" +
                       "\nT1 transpose" +
                       "\nT2 UTM" +
                       "\nT3 LTM" +
                       "\n\n#Matrix a#");
    System.out.println(a);
    System.out.println(a.transpose());
    System.out.println(a.upperDiagonal());
    System.out.println(a.lowerDiagonal());
    System.out.print(a.diagonalMatrix());
    System.out.println("##########" +
                       "\n\n#Matrix b#");
    System.out.println(b);
    System.out.println(b.transpose());
    System.out.println(b.upperDiagonal());
    System.out.println(b.lowerDiagonal());
    System.out.print(b.diagonalMatrix());
    System.out.println("====================\n");
  }
  
}
