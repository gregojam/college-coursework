////////////////////////////////////////////////////////////////////////////////////
//
//  C212 Spring 16
//  Homework 1
//
//  Released:  Thursday 1/21/16
//  Due:       Friday 1/29/16 11:59 PM  
//
//  Author  James Gregory     gregojam
//  Last Edited:  1/28/16
//
//
//  Directions:  provide code for unimplemented methods
//
//               ** The fractions do not need to be in a simplified form **
//               ** without being in simplified form it makes adding and subtracting easier **
//////////////////////////////////////////////////////////////////////////////////

public class Fraction {
    // Instance Fields declerations
    private int num; 
    private int denom;
    
    // Constructror - method that initializes class 
    // Paramaters
    // num   - numerator of fraction 
    // denom - denomenator of fraction 
    public Fraction(int num, int denom) {
        this.num = num;
        this.denom = denom; 
    }
    
    // @return value of numerator 
    public int num() {
        return num;
    }
    
    // @return value of denomenator 
    public int denom() {
        return denom; 
    }
    
    // add 2 fractions
    public Fraction add(Fraction other) { 
        int num = this.num() * other.denom() + other.num() * this.denom();
        int denom = this.denom() * other.denom();
        return new Fraction(num, denom); // return statments add initialy so file compiles
                                   // your will need to change the return statements 
    }
    
    // subtract two fractions 
    public Fraction minus(Fraction other) {
        int num = this.num() * other.denom() - other.num() * this.denom();
        int denom = this.denom() * other.denom();
        return new Fraction(num, denom);
    }
    
    // multiply two fractions 
    public Fraction multiply(Fraction other) {
        int num = this.num() * other.num();
        int denom = this.denom() * other.denom();
        return new Fraction(num, denom);
    }
    
    // divide two fractions 
    public Fraction divide(Fraction other) {
        int num = this.num() * other.denom();
        int denom = this.denom * other.num();
        return new Fraction(num, denom);
    }
    
    // returns decimal value of this fraction
    public double decimalVal() {
        // cast integer num and denom values as doubles before operating on in this method
      double num = (double) this.num();
      double denom = (double) this.denom();
     return this.num() / this.denom();   
    }
    
    
    public String toString() {
        return String.format("%s/%s", this.num(), this.denom());
    }
    
    // Test Client 
    public static void main(String[] args) {
        // creating a Fraction object from Class Fraction 
        // also known as in Instance 
        Fraction f1 = new Fraction(5, 10);
        Fraction f2 = new Fraction(1, 3);
        
        // example call of printing the value of two fractions multiplied 
        // f1.multiply(f2) returns a new Fraction object, so we can call its toString() method
        System.out.println( f1.multiply(f2).toString() );
        
    }
}