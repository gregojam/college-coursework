//This is simple program greets user

import java.util.Scanner;

public class Hello{
  
  private static void hiThere (String name){
    System.out.printf("Hello, %s! \n\n", name);
  }
  
  private static void byeBye (String name){
    System.out.printf("Goodbye, %s! \n\n", name);
  }
  
  public static void main (String[] ags){
    
    Scanner in = new Scanner(System.in);
    System.out.println("\n What is your name?");//asks for name
    String name = in.nextLine();
    String keepGoing = "true";
    while (keepGoing.equals("true")){
      keepGoing = "false";
      System.out.println("Are you coming or going?");//asks whether user is arriving or leaving
      String response = in.nextLine();
      if(response.equalsIgnoreCase("coming"))//determines proper greeting
        hiThere(name);
      else if (response.equalsIgnoreCase("going"))
        byeBye(name);
      else{
        System.out.println("That wasn't one of the choices... -_-");
        keepGoing = "true";
      }
    }
  }
}
           
