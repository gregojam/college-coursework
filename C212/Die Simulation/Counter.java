//////////////////////////////
//
//C212 Spring 2016
//Homework2
//
//Author: James Greogry
//Last Modified: 2/4/16
//
//////////////////////////////

import java.util.Scanner;

public class Counter{
  
  private final String name;
  private int count;
  
  // Constructor
  public Counter(String id){
    name = id;
    count = 0;
  }
  
  // increments count
  public void increment(){
    count++;
  }
  
  // returns count
  public int tally(){
    return count;
  }
  
  // prints name and count
  public String toString(){
    return name + ": " + count;
  }
  
  public static void main(String[] args){
    Counter c = new Counter("Steve");
    System.out.print("\nTest Block" + "\n====================" +
                       "\nT1 " + (c.name == "Steve") +
                       "\nT2 " + (c.tally() == 0) +
                       "\nT3 " + ((c.toString()).equals("Steve: 0")));
    c.increment();
    System.out.println("\nT4 " + (c.tally() == 1) +
                       "\nT5 " + ((c.toString()).equals("Steve: 1")) +
                       "\n====================\n");
  }
}
