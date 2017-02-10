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

abstract class Shape{
  
  private Color fillColor;
  private Color borderColor;
  private Boolean isFilled;
  private Point location;


  
  public Shape(Color fillColor, Color borderColor, int x, int y){
    this.fillColor = fillColor;
    this.borderColor = borderColor;
    location = new Point(x, y);
  }
  
  public Shape(Color fillColor, int x, int y){
    this.fillColor = fillColor;
    this.borderColor = Color.black;
    location = new Point(x, y);
  }
  
  public Shape(int x, int y){
    this.fillColor = Color.black;
    this.borderColor = Color.black;
    location = new Point(x, y);
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
  
  public void setLocation(int x, int y){
    location.setLocation(x, y);
  }
  
  public int getX(){
    return (int)location.getX();
  }
  
  public void setX(int x){
    location.move(x, getY());
  }
  
  public int getY(){
    return (int)location.getY();
  }
  
  public void setY(int y){
    location.move(getX(), y);
  }
  
  abstract Rectangle getRect();
  
  abstract void draw(Graphics g);
  
}