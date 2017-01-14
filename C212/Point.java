////////////////////////////////////////////////////////////////////////////////////
//
//  C212 Spring 16
//  Lab 2 Point Class 
//  
//  Released:  Thursday 1/21/16
//  Due:       Friday 1/29/16 11:59 PM
//
//  Author  James Gregory    gregojam
//  Last Edited:  1/28/16
//
//
//  Directions:  Impletment toString method and distanceTo method 
//  Note:        Will need to change reutrn values - added so code initialy compiles 
//               
//////////////////////////////////////////////////////////////////////////////////


import java.lang.Math;

public class Point {
    
    private int x;
    private int y;
    
    // Constructor method 
    public Point(int x, int y) {
        // this.x is the x as an instance field 
        // x is just the x local as a paramter to this method 
        this.x = x; 
        this.y = y;
    }
    
    // return this points x value 
    public int x() {
     return this.x;   
    }
    
    // return this points y value 
    public int y() {
     return this.y;   
    }
    
    // return distance from this point to other point  
    public double distanceTo(Point other) {
        return (double) Math.sqrt(Math.pow(this.x - other.x, 2) + Math.pow(this.y - other.y, 2));
    }
    
    // returns the point as a String 
    public String toString() {
        return String.format("(%s, %s)", this.x, this.y);
    }
 
    // test client 
    public static void main(String[] args) {
        // Instantiating 3 Objects of type Point 
        // Type is a Point - Variable Name - Creating new Point with new keywork 
        Point center = new Point(0,0);
        Point p1 = new Point(5, 10);
        Point p2 = new Point(3, 7);
        
        // creating a variable d1 that is didstance from Point Center to Point p1
        // call objects or class methods with . operator
        // distanceTO takes a parameter of type Point, so we passed in p1
        Double d1 = center.distanceTo(p1);
        
        System.out.println(d1);
    }
}