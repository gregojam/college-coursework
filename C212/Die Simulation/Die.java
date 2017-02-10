////////////////////
//
//C212 Spring 2016
//Homework2
//
//Author: James Gregory
//Last Modified: 2/5/16
//
////////////////////

import java.util.Random;

public class Die{
  
  private Random rand;
  
  // Constructor
  public Die(){
    rand = new Random();
  }
  
  // generates a random int in the set [1, 6]
  public int roll(){
    return rand.nextInt(6) + 1;
  }
  
  public static void main(String[] args){
    Die a = new Die();
    System.out.print("\nTest Block" + "\n====================");
    for(int i = 0; i < 10; i++){
      System.out.printf("\nT%s ", i); 
      System.out.print(a.roll());
    }
    System.out.println("\n====================\n");
                       
  }
}