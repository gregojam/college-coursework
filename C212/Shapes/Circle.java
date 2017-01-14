////////////////////
//
//C212 Spring 2016
//Lab 8
//
//Author: James Gregory
//Last Modified: 4/11/16
//
////////////////////

import java.awt.*;

class Circle extends Shape{
  
  private int radius;
  
  public Circle(Color fillColor, Color borderColor, int x, int y, int r){
    super(fillColor, borderColor, x, y);
    radius = r;
  }
  
  public Circle(Color fillColor, int x, int y, int r){
    super(fillColor, x, y);
    radius = r;
  }
  
  public Circle(int x, int y, int r){
    super(x, y);
    radius = r;
  }
  
  public void setRadius(int r){
    radius = Math.abs(r);
  }
  
  public int getRadius(){
    return radius;
  }
  
  public void draw(Graphics g){
    g.setColor(getFillColor());
    g.fillOval(getX(), getY(), radius * 2, radius * 2);
    g.setColor(getBorderColor());
    g.drawOval(getX(), getY(), radius * 2, radius * 2);
  }
  
  public String toString(){
    return "(" + getX() + ", " + getY() + ")";
  }
  
  public Rectangle getRect(){
    return new Rectangle(getX(), getY(), radius * 2, radius * 2);
  }
  
  public static void main(String[] args){
    Circle a = new Circle(Color.blue, Color.red, 0, 0, 2);
    System.out.println("\n\nTest Block\n====================");
    System.out.println("" +
                       a.getFillColor().equals(Color.blue) + "\n" +
                       a.getBorderColor().equals(Color.red) + "\n" +
                       a.isFilled() + "\n" +
                       (a.getRadius() == 2) + "\n" +
                       a.toString().equals("(0, 0)") + "\n" +
                       (a.getMoveX() == 1) + "\n" +
                       (a.getMoveY() == 1) + "\n");
    
    Circle b = new Circle(0, 0, 3);
    System.out.println("" +
                       b.getFillColor().equals(Color.white) + "\n" +
                       b.getBorderColor().equals(Color.black) + "\n" +
                       !b.isFilled() + "\n" +
                       (b.getRadius() == 3) + "\n" +
                       b.toString().equals("(0, 0)") + "\n");
    
    Circle c = new Circle(Color.green, 1, 2, 3);
    System.out.println("" +
                       c.getFillColor().equals(Color.green) + "\n" +
                       c.getBorderColor().equals(Color.black) + "\n" +
                       c.isFilled() + "\n" +
                       (c.getRadius()  == 3)+ "\n" +
                       c.toString().equals("(1, 2)") + "\n" +
                       "====================\n");
  }
}