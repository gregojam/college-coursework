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
import java.util.Random;

class Circle extends Shape{
  
  private int radius;
  private int moveX;
  private int moveY;
  private Random rand;
  
  public Circle(Color fillColor, Color borderColor, int x, int y, int r){
    super(fillColor, borderColor, x, y);
    radius = r;
    rand = new Random();
    moveX = rand.nextInt(11) - 5;
    moveY = -5;
  }
  
  public void setRadius(int r){
    radius = Math.abs(r);
  }
  
  public int getRadius(){
    return radius;
  }
      
  public void setMoveX(int m){
    moveX = m;
  }
  
  public int getMoveX(){
    return moveX;
  }
  
  public void setMoveY(int m){
    moveY = m;
  }
  
  public int getMoveY(){
    return moveY;
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
  
//  public static void main(String[] args){
//    Circle a = new Circle(Color.blue, Color.red, 0, 0, 2);
//    System.out.println("\n\nTest Block\n====================");
//    System.out.println("" +
//                       a.getFillColor().equals(Color.blue) + "\n" +
//                       a.getBorderColor().equals(Color.red) + "\n" +
//                       (a.getRadius() == 2) + "\n" +
//                       a.toString().equals("(0, 0)") + "\n" +
//                       (a.getMoveX() == 0) + "\n" +
//                       (a.getMoveY() == -3) + "\n");
//  }
}