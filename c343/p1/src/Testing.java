import java.awt.Color;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * Assorted JUnit tests for ColorTable.
 *  
 * To "turn off" any test, put double slashes ( "//" ) in front of the
 * "@Test" that precedes that method. For example, "//@Test".
 */

public class Testing {
  private static Color c0, c1, c2, c3, c4, c5, c6, c7;
  private static ColorTable table;
  
  @Test
  public void testPutAndGet() {
    table = new ColorTable(3, 6, Constants.LINEAR, 0.9);
    assertEquals(0, table.getSize());
    assertEquals(3, table.getCapacity());
    table.put(Color.BLACK, 5);
    assertEquals(1, table.getSize());
    assertEquals(5, table.get(Color.BLACK));
    table.put(Color.WHITE, 5);
    assertEquals(2, table.getSize());
    table.put(Color.RED, 5);
    table.put(Color.RED, 23);
    assertEquals(3, table.getSize());
    assertEquals(23, table.get(Color.RED));
  }

  @Test
  public void testPutAndGetAt() {
    table = new ColorTable(3, 2, Constants.LINEAR, 0.9);
    assertEquals(0, table.getSize());
    assertEquals(3, table.getCapacity());
    assertEquals(true, table.isEmpty());
    table.put(Color.BLACK, 5);
    assertEquals(1, table.getSize());
    assertEquals(3, table.getCapacity());
    assertEquals(false, table.isEmpty());
    //assertEquals(0, ColorTable.getNumCollisions());
    table.put(Color.WHITE, 5);
    table.put(Color.RED, 5);
    table.put(Color.BLUE, 5);
    assertEquals(7, table.getCapacity());
    table.put(Color.RED, 23);
    assertEquals(7, table.getCapacity());
    assertEquals(23, table.getCountAt(6));
    assertEquals(5, table.getCountAt(0));
  }

  @Test
  public void testImplicitCounts() {
    table = new ColorTable(3, 6, Constants.LINEAR, 0.9);
    assertEquals(0, table.get(Color.BLUE));
    assertEquals(0, table.get(Color.PINK));
    assertEquals(0, table.get(Color.CYAN));
    assertEquals(0, table.getSize());
  }

  @Test
  public void testIncrementAndGet() {
    table = new ColorTable(3, 4, Constants.LINEAR, 0.9);
    table.increment(Color.RED);
    table.increment(Color.RED);
    assertEquals(2, table.get(Color.RED));
    for (int i = 0; i < 100; i++)
      table.increment(Color.BLUE);
    assertEquals(100, table.get(Color.BLUE));
    assertEquals(2, table.get(Color.RED));
  }

  @Test
  public void testIncrementTruncated() {
    table = new ColorTable(3, 1, Constants.LINEAR, 0.9);  // just using 3 bits for a color
    c1 = new Color(0xf0, 0xe0, 0xd0);
    c2 = new Color(0xff, 0xee, 0xdd);
    c3 = new Color(0xd1, 0xf2, 0xe3);

    table.put(c1, 5);
    assertEquals(5, table.get(c1));
    table.increment(c2);
    table.increment(c3);
    assertEquals(7, table.get(c1));
  }

  @Test
  public void testIncrementAndResizing() {
    table = new ColorTable(3, 6, Constants.LINEAR, 0.8);
    table.increment(Color.BLACK);
    assertEquals(1, table.get(Color.BLACK));
    table.increment(Color.WHITE);
    assertEquals(1, table.get(Color.WHITE));
    table.increment(Color.BLACK);
    assertEquals(2, table.get(Color.BLACK));
    table.increment(Color.BLACK);
    assertEquals(3, table.get(Color.BLACK));
    table.increment(Color.WHITE);
    assertEquals(2, table.get(Color.WHITE));
    table.increment(Color.BLACK);
    assertEquals(4, table.get(Color.BLACK));

    // System.out.println(table);

    // Check status prior to rehashing.
    assertEquals(2, table.getSize());
    assertEquals(3, table.getCapacity());
    // This increment will trigger a rehash event.
    table.increment(Color.RED);
    // Check status after rehashing.
    assertEquals(3, table.getSize());
    assertEquals(7, table.getCapacity());

    // System.out.println(table);

    // Check the final frequency counts.
    assertEquals(1, table.get(Color.RED));
    assertEquals(4, table.get(Color.BLACK));
    assertEquals(2, table.get(Color.WHITE));
  }

  @Test
  public void testQuadraticProbing() {
    table = new ColorTable(13, 8, Constants.QUADRATIC, 0.49);
    // For a tableSize of 13, these keys all collide.
    c0 = new Color(0);
    c1 = new Color(13);
    c2 = new Color(2 * 13);
    c3 = new Color(3 * 13);
    c4 = new Color(4 * 13);
    c5 = new Color(5 * 13);
    c6 = new Color(6 * 13);
    c7 = new Color(7 * 13);
    table.put(c0, 1);
    table.put(c1, 2);
    table.put(c2, 3);
    assertEquals(3, table.getSize());
    assertEquals(13, table.getCapacity());
    assertEquals(3, table.getCountAt(4));
    table.put(c3, 4);
    assertEquals(4, table.getCountAt(9));
    table.put(c4, 5);
    assertEquals(5, table.getCountAt(3));
    table.put(c5, 6);
    assertEquals(6, table.getCountAt(12));
    assertEquals(6, table.getSize());
    assertEquals(13, table.getCapacity());
  }

  @Test
  public void testQuadraticProbingAndResizing() {
    testQuadraticProbing(); // This sets up the table to just before resizing.
    assertEquals(0, table.getCountAt(7)); // This fails if you rehash too soon. 
    assertEquals(6, table.getSize());
    assertEquals(13, table.getCapacity());

    table.put(c5, 999); // This is an update -- do not rehash now.
    assertEquals(0, table.getCountAt(7)); // Again, this fails if you rehash too soon. 
    assertEquals(6, table.getSize());
    assertEquals(13, table.getCapacity());

    // The next operation should force a resize from 13 to 31.
    table.put(c6, 7);
    assertEquals(7, table.getCountAt(16));
    table.put(c7, 8);
    assertEquals(8, table.getCountAt(29));
    assertEquals(8, table.getSize());
    assertEquals(31, table.getCapacity());
  }

  @Test
  public void testLowLoadFactor() {
    table = new ColorTable(3, 4, Constants.LINEAR, 0.1);
    assertEquals(0, table.getSize());
    assertEquals(3, table.getCapacity());
    assertEquals(true, table.getLoadFactor() < 0.1);
    table.put(Color.BLACK, 5);
    assertEquals(1, table.getSize());
    assertEquals(19, table.getCapacity());
    assertEquals(true, table.getLoadFactor() < 0.1);
    assertEquals(5, table.get(Color.BLACK));
    table.put(Color.WHITE, 5);
    assertEquals(2, table.getSize());
    assertEquals(43, table.getCapacity());
    assertEquals(true, table.getLoadFactor() < 0.1);
    for (int c = 0xFF; c >= 0xF0; c--)
      table.put(new Color(c, c, c), c);
    assertEquals(0xF0, table.get(Color.WHITE));
    assertEquals(2, table.getSize());
    for (int c = 0x0F; c <= 0xFF; c += 0x10)
      table.put(new Color(c, c, c), c);
    assertEquals(211, table.getCapacity());
    assertEquals(true, table.getLoadFactor() < 0.1);
  }

  @Test
  public void testIterator() {
    table = new ColorTable(13, 2, Constants.LINEAR, 0.49);
    for (int i = 0; i < 20 * 17; i += 17)
      table.put(new Color(i * i),  i);
    assertEquals(9, table.getSize());
    int[] expected = { 272, 119, 102, 0, 0, 289, 0, 306, 0, 0, 323, 221, 255, 238 };
    Iterator it = table.iterator();
    int k = 0;
    while (it.hasNext() && k < expected.length) {
      assertEquals(expected[k], it.next());
      k++;
    }
    
    table = new ColorTable(13, 2, Constants.QUADRATIC, 0.49);
    for (int i = 0; i < 20 * 17; i += 17)
      table.put(new Color(i * i),  i);
    assertEquals(9, table.getSize());
    it = table.iterator();
    k = 0;
    while (it.hasNext() && k < expected.length) {
      assertEquals(expected[k], it.next());
      k++;
    }
  }
}