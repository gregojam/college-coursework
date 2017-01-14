//////////////////////////////
//
//C212 Spring 2016
//Lab 6
//
//Author: James Gregory
//Last Modified: 2/22/16
//
//////////////////////////////

class Frequency{
  
  private String word;
  private int count;
  
  // contructor
  public Frequency(String word){
    this.word = word;
    count = 1;
  }
  
  // getters
  public String getWord(){
    return word;
  }
  
  public int getCount(){
    return count;
  }
  // end getters
  
  // increments count
  public void add1(){
    count += 1;
  }
  
  public static void main(String[] args){
    Frequency a = new Frequency("Hello");
    System.out.println(a.getWord() + a.getCount());
    a.add1();
    a.add1();
    System.out.println(a.getWord() + a.getCount());
  }
}