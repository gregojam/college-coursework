//////////////////////////////
//
//C212 Spring 2016
//HW2
//
//Author: James Gregory
//Last Modified: 2/4/16
//
//////////////////////////////

public class Homework2{
  
  // determines if a string is a palindrome
  public static boolean isPalindrome(String s){
    if (s.length() <= 1)
      return true;
    else if ((s.toLowerCase()).charAt(0) == (s.toLowerCase()).charAt(s.length() - 1))
      return isPalindrome(s.substring(1, s.length() - 1));
    else
      return false;
  }
  
  // determines if a string is alphabetical
  public static boolean isSorted(String s){
    if (s.length() == 1)
      return true;
    else if ((s.toLowerCase()).charAt(0) <= (s.toLowerCase()).charAt(1))
      return isSorted(s.substring(1));
    else
      return false;
  }
  
  // prints an n by n box of asterisks
  public static void border(int n){
    if (n == 1)
      System.out.println("*");
    else if (n > 1){
      for(int i = 0; i < n; i++){
        for(int j = 0; j < n; j++){
          if (i % (n-1) ==0 || j % (n-1) == 0)
            System.out.print("* ");
          else
            System.out.print("  ");
        }
        System.out.println("");
      }
    }
    else if (n == 0);
      // Do Nothing
    else
      border(n - n * 2);
  }
  
  // prints a T of asterisks
  public static void T(int n){
    if (n == 1)
      System.out.println("*");
    else if (n > 1){
      for(int i = 0; i < n; i++){
        for(int j = 0; j < n; j++){
          if (i == 0 || j == n / 2 || (j == n / 2 - 1 && n % 2 == 0))
            System.out.print("* ");
          else
            System.out.print("  ");
        }
        System.out.println();
      }
    }
    else if (n == 0);
      // Do Nothing
    else
      T(n - n * 2);
  }
  
  
  public static void main(String[] args){
    //Test Block Begin
    System.out.println("\nTest Block" + "\n====================" +
                       "\nT1 " + isPalindrome("racecar")  +
                       "\nT2 " + isPalindrome("ToOt")  +
                       "\nT3 " + isPalindrome("a")  +
                       "\nT4 " + !isPalindrome("ab")  +
                       "\nT5 " + !isPalindrome("NotAPalindrome") +
                       "\nT6 " + isSorted("abcdefg") +
                       "\nT7 " + isSorted("AefFgtWWx") +
                       "\nT8 " + isSorted("a") +
                       "\nT9 " + !isSorted("alksjhdf"));
    System.out.println("T10 5 border"); border(5);
    System.out.println("T11 4 border"); border(-4);
    System.out.println("T12 1 border"); border(1);
    System.out.println("T13 0 border"); border(0);
    System.out.println("T14 8 'T'"); T(8);
    System.out.println("T15 7 'T'"); T(7);
    System.out.println("T16 4 'T'"); T(-4);
    System.out.println("T17 3 'T'"); T(-3);
    System.out.println("T18 1 'T'"); T(1);
    System.out.println("T19 0 'T'"); T(0);
    
    System.out.println("====================\n");
    //Test Block End
  }
}