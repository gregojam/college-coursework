import java.awt.Color;
import java.util.Random;

/**
 * An enumeration of the five possible colors used to create tiles in the game.
 * Operations are provided to access the underlying java.awt.Color object and to
 * select a water color at random.
 */

public enum WaterColor {
  BLUE(Color.BLUE),       // blue
  RED(Color.RED),         // red
  CYAN(Color.CYAN),       // cyan
  PINK(Color.PINK),       // pink
  YELLOW(Color.YELLOW);   // yellow

  private Color color;
  private static Random gen = new Random();
  private static int n = values().length;

  private WaterColor(Color color) {
    this.color = color;
  }

  /**
   * Returns the java.awt.Color object associated with this water color.
   */
  public Color get() {
    return color;
  }
  
  /**
   * Returns a water color, selected at random and with equal probability.
   */
  public static WaterColor pickOne() {
    return values()[gen.nextInt(n)];
  }
  
  /**
   * Returns a water color that is different from thisOne, selected at random and
   * with equal probability.
   */
  public static WaterColor pickOneExcept(WaterColor thisOne) {
    WaterColor color;
    do
      color = pickOne();
    while (color == thisOne);
    return color;
  }
  
  /**
   * Returns a string representation of this water color.
   */
  public String toString() {
    return super.toString().toLowerCase();
  }
  
  /**
   * Simple testing.
   */
  public static void main(String... args) {
    for (WaterColor color : values())
      System.out.println(color + "\t" + pickOneExcept(color));
  }
}

