import static org.junit.Assert.*;

import org.junit.Test;

/**
 * JUnit tests for all TODO methods.
 */

public class Testing {
  
  @Test
  public void testOnBoard() {
    assertFalse(new Coord(3, 4).onBoard(4));
    assertTrue(new Coord(3, 4).onBoard(5));
  }

}