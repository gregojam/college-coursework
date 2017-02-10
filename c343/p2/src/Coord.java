import java.util.ArrayList;
import java.util.List;

/**
 * Coord represents an (x,y)-coordinate on a 2D board. The origin, (0,0) is
 * assumed to be in the upper left corner. X-coordinates increase from left
 * to right. Y-coordinates increase from top to bottom. Operations are provided
 * to easily locate neighboring coordinates (in the NSEW compass directions)
 * on a board.
 * 
 * @author James Gregory
 */

public class Coord implements Comparable<Coord> {
  /**
   *  The upper left corner of the board.
   */
  public static Coord ORIGIN = new Coord(0, 0);
  
  private int x, y;

  /**
   * Constructs a new Coord that is a copy of the given Coord.
   */
  public Coord(Coord coord) {
    this(coord.x, coord.y);
  }

  /**
   * Constructs a new Coord representing (x,y).
   */
  public Coord(int x, int y) {
    this.x = x;
    this.y = y;
  }

  /**
   * Returns the Coord that is directly above (i.e., north of) this one.
   */
  public Coord up() {
    return new Coord(x, y - 1);
  }

  /**
   * Returns the Coord that is directly below (i.e., south of) this one.
   */
  public Coord down() {
    return new Coord(x, y + 1);
  }

  /**
   * Returns the Coord that is immediately to the left (i.e., west)
   * of this one.
   */
  public Coord left() {
    return new Coord(x - 1, y);
  }

  /**
   * Returns the Coord that is immediately to the right (i.e., east)
   * of this one.
   */
  public Coord right() {
    return new Coord(x + 1, y);
  }

  /**
   * TODO
   * 
   * Returns true iff this Coord is visible on a board of the given size.
   */
  public boolean onBoard(int size) {
    return (x >=0 && x < size && y >= 0 && y < size);
  }

  /**
   * TODO
   * 
   * Returns a list of the immediate board coordinates of this Coord's north,
   * south, east, and west neighbors.
   */
  public List<Coord> neighbors(int size) {
	List<Coord> ns = new ArrayList<>();
	if(up().onBoard(size))
		ns.add(up());
	if(down().onBoard(size))
		ns.add(down());
	if(right().onBoard(size))
		ns.add(right());
	if(left().onBoard(size))
		ns.add(left());
    return ns;
  }

  /**
   * Returns true iff the (x,y)-coordinates of the given object match this Coord's
   * (x,y)-coordinates.
   */
  public boolean equals(Object obj) {
    if (obj instanceof Coord) {
      Coord that = (Coord) obj;
      return that.x == this.x && that.y == this.y;
    }
    return false;
  }

  /**
   * Coords are ordered first on the x-coordinate and then on the y-coordinate.
   */
  public int compareTo(Coord that) {
    if (this.x == that.x)
      return this.y - that.y;
    return this.x - that.x;
  }

  /**
   * Returns the x-coordinate.
   */
  public int getX() {
    return x;
  }

  /**
   * Returns the y-coordinate.
   */
  public int getY() {
    return y;
  }

  /**
   * TODO
   * 
   * Pre-hashing: pack this coordinate into an int, so that the key space is
   * as uniformly distributed among the range of integers as possible.
   */
  public int hashCode() {
    return (x << 5) - x + y;
  }

  /**
   * Returns this Coord as a string of the form (x, y).
   */
  public String toString() {
    return "(" + x + ", " + y + ")"; 
  }
  
  /**
   * Simple testing.
   */
  public static void main(String... args) {
    Coord someCoord = new Coord(2, 1);
    System.out.println("someCoord = " + someCoord);
    System.out.println("someCoord.hashCode() = " + someCoord.hashCode());
    System.out.println("someCoord.onBoard(4) = " + someCoord.onBoard(4));
    System.out.println("neighbors on a 3x3 board = " + someCoord.neighbors(3));
    System.out.println("neighbors on a 4x4 board = " + someCoord.neighbors(4));   
    System.out.println();
    
    someCoord = ORIGIN;
    System.out.println("someCoord = " + someCoord);
    System.out.println("someCoord.hashCode() = " + someCoord.hashCode());
    System.out.println("someCoord.onBoard(3) = " + someCoord.onBoard(3));
    System.out.println("neighbors on a 3x3 board = " + someCoord.neighbors(3));
    System.out.println("neighbors on a 1x1 board = " + someCoord.neighbors(1));   
    System.out.println();

    someCoord = new Coord(5, 5);
    System.out.println("someCoord = " + someCoord);
    System.out.println("someCoord.hashCode() = " + someCoord.hashCode());
    System.out.println("someCoord.onBoard(5) = " + someCoord.onBoard(5));
    System.out.println("neighbors on a 3x3 board = " + someCoord.neighbors(3));
    System.out.println("neighbors on a 6x6 board = " + someCoord.neighbors(6));
  }
}
