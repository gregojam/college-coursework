////////////////////
//
//C212 Spring 2016
//Lab 8
//
//Author: James Gregory
//Last Modified: 4/11/16
//
////////////////////

import java.awt.Color;

class Square extends Rectangle2{
  
  public Square(Color fillColor, Color borderColor,
                int x, int y, int w){
    super(fillColor, borderColor, x, y, w, w);
  }
  
  public Square(Color fillColor, int x, int y, int w){
    super(fillColor, x, y, w, w);
  }
  
  public Square(int x, int y, int w){
    super(x, y, w, w);
  }
  
  public static void main(String[] args){
    Square a = new Square(Color.blue, Color.red, 0, 0, 2);
    System.out.println("\n\nTest Block\n====================");
    System.out.println("" +
                       a.getFillColor().equals(Color.blue) + "\n" +
                       a.getBorderColor().equals(Color.red) + "\n" +
                       a.isFilled() + "\n" +
                       (a.getWidth() == 2) + "\n" +
                       (a.getHeight() == 2) + "\n" +
                       a.toString().equals("(0, 0)") + "\n");
    
    Square b = new Square(0, 0, 3);
    System.out.println("" + 
                       b.getFillColor().equals(Color.white) + "\n" +
                       b.getBorderColor().equals(Color.black) + "\n" +
                       !b.isFilled() + "\n" +
                       (b.getWidth() == 3) + "\n" +
                       (b.getHeight() == 3) + "\n" +
                       b.toString().equals("(0, 0)") + "\n");
    
    Square c = new Square(Color.green, 1, 2, 4);
    System.out.println("" +
                       c.getFillColor().equals(Color.green) + "\n" +
                       c.getBorderColor().equals(Color.black) + "\n" +
                       c.isFilled() + "\n" +
                       (c.getWidth() == 4) + "\n" +
                       (c.getHeight() == 4) + "\n" +
                       c.toString().equals("(1, 2)") + "\n" +
                       "====================\n");
  }
}