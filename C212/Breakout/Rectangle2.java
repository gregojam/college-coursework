////////////////////
//
//C212 Spring 2016
//Lab 8
//
//Author: James Gregory
//Last Modified: 4/22/16
//
////////////////////

import java.awt.*;

class Rectangle2 extends Shape{
  
  private int width;
  private int height;
  
  public Rectangle2(Color fillColor, Color borderColor,
                   int x, int y, int w, int h){
    super(fillColor, borderColor, x, y);
    width = w;
    height = h;
  }
  
  public Rectangle2(int x, int y, int w, int h){
    super(x, y);
    width = w;
    height = h;
  }
  
  public void setWidth(int w){
    width = w;
  }
  
  public int getWidth(){
    return width;
  }
  
  public void setHeight(int h){
    height = h;
  }
  
  public int getHeight(){
    return height;
  }
  
  public void draw(Graphics g){
    g.setColor(getFillColor());
    g.fillRect(getX(), getY(), width, height);
    g.setColor(getBorderColor());
    g.drawRect(getX(), getY(), width, height);
  }
  
  public String toString(){
    return "(" + getX() + ", " + getY() + ")";
  }
  
  public Rectangle getRect(){
    return new Rectangle(getX(), getY(), getWidth(), getHeight());
  }
  
//  public static void main(String[] args){
//    Rectangle2 a = new Rectangle2(Color.blue, Color.red, 0, 0, 2, 3);
//    System.out.println("\n\nTest Block\n====================");
//    System.out.println("" +
//                       a.getFillColor().equals(Color.blue) + "\n" +
//                       a.getBorderColor().equals(Color.red) + "\n" +
//                       (a.getWidth() == 2) + "\n" +
//                       (a.getHeight() == 3) + "\n" +
//                       a.toString().equals("(0, 0)") + "\n");
//    
//    Rectangle2 b = new Rectangle2(0, 0, 2, 3);
//    System.out.println("" + 
//                       b.getFillColor().equals(Color.black) + "\n" +
//                       b.getBorderColor().equals(Color.black) + "\n" +
//                       (b.getWidth() == 2) + "\n" +
//                       (b.getHeight() == 3) + "\n" +
//                       b.toString().equals("(0, 0)") + "\n");
//  }
}