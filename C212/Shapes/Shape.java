////////////////////
//
//C212 Spring 2016
//Lab 8
//
//Author: James Gregory
//Last Modified: 34/11/16
//
////////////////////

import java.awt.*;
import java.util.Random;

abstract class Shape{
  
  private Color fillColor;
  private Color borderColor;
  private Boolean isFilled;
  private Point location;
  private Random random = new Random();
  private int moveX;
  private int moveY;
  
  public Shape(Color fillColor, Color borderColor, int x, int y){
    this.fillColor = fillColor;
    this.borderColor = borderColor;
    location = new Point(x, y);
    moveX = random.nextInt(9) - 4;
    if(moveX != 0)
      moveX /= Math.abs(moveX);
    moveY = random.nextInt(9) - 4;
    if(moveY != 0)
      moveY /= Math.abs(moveY);
  }
  
  public Shape(Color fillColor, int x, int y){
    this.fillColor = fillColor;
    this.borderColor = Color.black;
    location = new Point(x, y);
    moveX = random.nextInt(9) - 4;
    if(moveX != 0)
      moveX /= Math.abs(moveX);
    moveY = random.nextInt(9) - 4;
    if(moveY != 0)
      moveY /= Math.abs(moveY);
  }
  
  public Shape(int x, int y){
    this.fillColor = Color.white;
    this.borderColor = Color.black;
    location = new Point(x, y);
    moveX = random.nextInt(9) - 4;
    if(moveX != 0)
      moveX /= Math.abs(moveX);
    moveY = random.nextInt(9) - 4;
    if(moveY != 0)
      moveY /= Math.abs(moveY);
  }
  
  public void setFillColor(Color c){
    fillColor = c;
  }
  
  public Color getFillColor(){
    return fillColor;
  }
  
  public void setBorderColor(Color c){
    borderColor = c;
  }
  
  public Color getBorderColor(){
    return borderColor;
  }
    
  public void moveLoc(int dx, int dy){
    location.setLocation((int)location.getX() + dx,
                         (int)location.getY() + dy);
  }
  
  public Point getLocation(){
    return location;
  }
  
  public int getX(){
    return (int)location.getX();
  }
  
  public int getY(){
    return (int)location.getY();
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
  
  public boolean isFilled(){
    if(!fillColor.equals(Color.white))
      return true;
    else
      return false;
  }
  
  abstract Rectangle getRect();
  
  abstract void draw(Graphics g);
  
}