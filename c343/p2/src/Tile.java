/**
 * Tile is a logical representation of one tile on the game board. A tile
 * knows its location and its color.
 */

public class Tile {
  private Coord coord;
  private WaterColor color;
  
  /**
   * Constructs a tile at the origin with a randomly selected color.
   */
  public Tile() {
    this(Coord.ORIGIN);
  }
  
  /**
   * Constructs a tile at the given coordinate with a randomly selected color.
   */
  public Tile(Coord coord) {
    this(coord, WaterColor.pickOne());
  }
  
  /**
   * Constructs a tile at the given coordinate and with the given color.
   */
  public Tile(Coord coord, WaterColor color) {
    this.coord = coord;
    this.color = color;
  }
  
  /**
   * Returns the coordinate associated with this tile.
   */
  public Coord getCoord() {
    return coord;
  }
  
  /**
   * Returns the color associated with this tile.
   */
  public WaterColor getColor() {
    return color;
  }
  
  /**
   * Changes the color of this tile to the given color.
   */
  public void setColor(WaterColor color) {
    this.color = color;
  }
  
  /**
   * Returns true iff the given object is a tile at the same coordinate and
   * with the same color.
   */
  public boolean equals(Object obj) {
    if (obj instanceof Tile) {
      Tile that = (Tile) obj;
      return this.coord.equals(that.coord) && this.color == that.color;
    }
    return false;
  }
  
  /**
   * Returns a string representation of this tile. 
   */
  public String toString() {
    return "Tile[" + getCoord() + ", " + getColor() + "]";
  }
  
  /**
   * Simple testing.
   */
  public static void main(String... args) {
    Tile someTile = new Tile(new Coord(2, 3), WaterColor.pickOne());
    System.out.println(someTile);
  }
}
