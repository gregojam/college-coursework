import static org.junit.Assert.*;
import org.junit.Test;

/**
 * The following table shows the first ten numbers in the
 * Fibonacci sequence:
 * 
 *      n  :  0  1  2  3  4  5  6  7  8  9 ...
 *  Fib(n) :  1  1  2  3  5  8 13 21 34 55 ...
 * 
 * @author James Gregory
 */

public class Fibonacci {

  /**
   * TODO: Implement the recursive definition directly.
   */
  public static int fib1(int n) {
	  if(n < 2)
		  return 1;
	  else
		  return fib1(n-1) + fib1(n-2);
  }

  /**
   * TODO: Implement recursively by calling a tail-recursive helper.
   */
  public static int fib2(int n){
    return fibHelp(1, 1, n);
  }
  
  private static int fibHelp(int x, int y, int n){
	  if(n > 0)
		  return fibHelp(y, x + y, --n);
	  return x;
  };

  /**
   * TODO: Run this class as an application.
   */
  public static void main(String... args) {
    assert fib1(9) == 55;
    assert fib1(10) == 89;
    assert fib1(0) == 1;
    assert fib1(1) == 1;
    assert fib2(9) == 55;
    assert fib2(10) == 89;
    assert fib2(0) == 1;
    assert fib2(1) == 1;
  }
  
  /**
   * TODO: Run this class as a JUnit test. Add additional tests to
   * the following methods.
   */
  
  @Test
  public void testFib1() {
    assertEquals(55, fib1(9));
    assertEquals(89, fib1(10));
    assertEquals(1, fib1(0));
    assertEquals(1, fib1(1));
  }
  
  @Test
  public void testFib2() {
    assertEquals(55, fib2(9));
    assertEquals(89, fib2(10));
    assertEquals(1, fib2(1));
    assertEquals(1, fib2(0));
  }
}
