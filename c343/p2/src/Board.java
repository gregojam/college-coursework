import java.util.HashMap;
import java.util.Map;

/**
 * A Board represents the current state of the game. Boards know their dimension, 
 * the collection of tiles that are inside the current flooded region, and those tiles 
 * that are on the outside.
 * 
 * @author James Gregory
 */

public class Board {
  private Map<Coord, Tile> inside, border, outside;
  private int size;
  
  /**
   * Constructs a square game board of the given size, initializes the list of 
   * inside tiles and border tiles to include just the tile in the upper left corner,
   * and puts all the other tiles in the outside list.
   */
  public Board(int size) {
    // A tile is either inside or outside the current flooded region.
	// Some inside tiles are also on the border between inside and outside.
    inside = new HashMap<>();
    border = new HashMap<>();
    outside = new HashMap<>();
    this.size = size;
    for (int y = 0; y < size; y++)
      for (int x = 0; x < size; x++) {
        Coord coord = new Coord(x, y);
        outside.put(coord, new Tile(coord));
      }
    // Move the corner tile into the flooded region and run flood on its color.
    Tile corner = outside.remove(Coord.ORIGIN);
    inside.put(Coord.ORIGIN, corner);
    border.put(Coord.ORIGIN, corner);
    flood(corner.getColor());
  }
  
  /**
   * Returns the tile at the specified coordinate.
   */ 
  public Tile get(Coord coord) {
    if (outside.containsKey(coord))
      return outside.get(coord);
    return inside.get(coord);
  }
  
  /**
   * Returns the size of this board.
   */
  public int getSize() {
    return size;
  }
  
  /**
   * TODO
   * 
   * Returns true iff all tiles on the board have the same color.
   */
  public boolean fullyFlooded() {
    return inside.size() == size * size;
  }
  
  /**
   * TODO
   * 
   * Updates this board by changing the color of the current flood region 
   * and extending its reach.
   * 
   * 
   * flood keeps track of tiles that are on the border of the inside (with neighbors
   * on the outside), and only checks those tile for concurring neighbors that belong
   * on the inside.
   */
  public void flood(WaterColor color) {
	  LinkedQueue<Coord> borderRemove = new LinkedQueue<>();
	  LinkedQueue<Tile> borderAdd = new LinkedQueue<>();
	  java.util.List<Coord> neighs;
	  Tile aTile;
	  boolean onBorder;

	  // Change inside color
	  for(Tile t : inside.values())
		  t.setColor(color);
	  
	  // Check border tiles for expansion
	  for(Tile t : border.values()){
		  neighs = t.getCoord().neighbors(size);
		  onBorder = false;
		  for(Coord c : neighs){
			  if(outside.containsKey(c)){
				  if(get(c).getColor().equals(t.getColor()))
					  borderAdd.enqueue(outside.remove(c));
				  else
					  onBorder = true;
			  }
		  }
		  if(!onBorder)
			  borderRemove.enqueue(t.getCoord());
	  }
	  
	  //Update the border and inside with new Tiles
	  while(!borderAdd.isEmpty()){
		  aTile = borderAdd.dequeue();
		  neighs = aTile.getCoord().neighbors(size);
		  onBorder = false;
		  for(Coord c : neighs){
			  if(outside.containsKey(c)){
				  if(get(c).getColor().equals(aTile.getColor()))
				  borderAdd.enqueue(outside.remove(c));
				  else
					  onBorder = true;
			  }
		  }
		  if(onBorder)
			  border.put(aTile.getCoord(), aTile);
		  inside.put(aTile.getCoord(), aTile);
	  }
	  
	  //Remove border Tiles that are no longer on the border
	  while(!borderRemove.isEmpty())
		  border.remove(borderRemove.dequeue());
  } 
  
  /**
   * TODO
   * 
   * Explore a variety of algorithms for handling a flood. Use the same interface 
   * as for flood above, but add an index so your methods are named flood1,
   * flood2, ..., and so on. Then, use the batchTest() tool in Driver to run game
   * simulations and then display a graph showing the run times of all your different 
   * flood functions. Do not delete your flood code attempts. For each variety of 
   * flood, including the one above that you eventually settle on, write a comment
   * that describes your algorithm in English. For those implementations that you
   * abandon, state your reasons.
   */
  
  /**
   * flood1 checks all of the inside Tiles' neighbors to find where the flood zone
   * should extend to. It then continues checking known extension Tiles for further
   * extension Tiles until no more are found, and places each extension Tile in the 
   * inside.
   * 
   * This algorithm was abandoned, because it checks too many tiles unnecessarily,
   * adding an excessive amount of runtime.
   */
  public void flood1(WaterColor color) {
	  LinkedQueue<Tile> floodRegion = new LinkedQueue<>();
	  Tile aTile;
	  java.util.List<Coord> neighs;
	  
	  for(Tile t : inside.values())
		  t.setColor(color);
	  
	  for(Tile t : inside.values()){
		  neighs = t.getCoord().neighbors(size);
		  for(Coord c : neighs){
			  if(outside.containsKey(c) && get(c).getColor().equals(t.getColor())){
				  floodRegion.enqueue(outside.remove(c));
			  }
		  }
	  }
	  
	  while(!floodRegion.isEmpty()){
		  aTile = floodRegion.dequeue();
		  neighs = aTile.getCoord().neighbors(size);
		  for(Coord c : neighs){
			  if(outside.containsKey(c) && get(c).getColor().equals(aTile.getColor()))
				  floodRegion.enqueue(outside.remove(c));
		  }
		  inside.put(aTile.getCoord(), aTile);
	  }

  }
  
  /**
   * flood2 checks the Tiles on the outside to extend the flood zone. To ensure the flood
   * zone is fully extended, the function continuously rechecks the outside Tiles until
   * the inside doesn't grow in size.
   * 
   *This algorithm was abandoned, because it also checks too many tiles unnecessarily,
   * adding an excessive amount of runtime.
   */
  public void flood2(WaterColor color) {
	  LinkedQueue<Tile> floodRegion = new LinkedQueue<>();
	  java.util.List<Coord> neighs;
	  boolean keepGoing;
	  int startSize;
	  
	  for(Tile t : inside.values())
		  t.setColor(color);
	  
	  keepGoing = true;
	  while(keepGoing){
		  startSize = inside.size();
		  for(Tile t : outside.values()){
			  neighs = t.getCoord().neighbors(size);
			  for(Coord c : neighs){
				  if(inside.containsKey(c) && get(c).getColor().equals(t.getColor())){
					  inside.put(t.getCoord(), t);
					  floodRegion.enqueue(t);
				  }
			  }
		  }
		  if(inside.size() == startSize)
			  keepGoing = false;
	  }
	  
	  while(!floodRegion.isEmpty())
		  outside.remove(floodRegion.dequeue().getCoord());
  }
  
  /**
   * Runs a simulation of flood for a given color without changing the actual board.
   * Returns the size of the new inside, if such a move were chosen.
   */
  private int simFlood(WaterColor color){
	  Map<Coord, Tile> simIn = new HashMap<>();
	  Map<Coord, Tile> simBorder = new HashMap<>();
	  Map<Coord, Tile> simOut = new HashMap<>();
	  LinkedQueue<Coord> borderRemove = new LinkedQueue<>();
	  LinkedQueue<Tile> borderAdd = new LinkedQueue<>();
	  java.util.List<Coord> neighs;
	  Tile aTile;
	  boolean onBorder;
	  Coord tmpC;
	  Tile tmpT;
	  
	  // Provide new get function to correspond with sim board
	  java.util.function.Function<Coord, Tile> simGet = (coord) -> {
		  if(simOut.containsKey(coord))
			  return simOut.get(coord);
		  return simIn.get(coord);
	  };
	  
	  // Copy inside to simIn
	  for(Tile t : inside.values()){
		  tmpC = new Coord(t.getCoord());
		  tmpT = new Tile(tmpC, t.getColor());
		  simIn.put(tmpC, tmpT);
	  }
	  
	  // Copy border to simBorder
	  for(Tile t : border.values()){
		  tmpC = new Coord(t.getCoord());
		  tmpT = new Tile(tmpC, t.getColor());
		  simBorder.put(tmpC, tmpT);
	  }
	  
	  // Copy outside to simOut
	  for(Tile t : outside.values()){
		  tmpC = new Coord(t.getCoord());
		  tmpT = new Tile(tmpC, t.getColor());
		  simOut.put(tmpC, tmpT);
	  }

	  // Change simBorder color for comparisons
	  for(Tile t : simBorder.values())
		  t.setColor(color);
	  
	  // Check simBorder for expansion
	  for(Tile t : simBorder.values()){
		  neighs = t.getCoord().neighbors(size);
		  onBorder = false;
		  for(Coord c : neighs){
			  if(simOut.containsKey(c)){
				  if(simGet.apply(c).getColor().equals(t.getColor()))
					  borderAdd.enqueue(simOut.remove(c));
				  else
					  onBorder = true;
			  }
		  }
		  if(!onBorder)
			  borderRemove.enqueue(t.getCoord());
	  }
	  
	  //Update simIn with new Tiles
	  while(!borderAdd.isEmpty()){
		  aTile = borderAdd.dequeue();
		  neighs = aTile.getCoord().neighbors(size);
		  onBorder = false;
		  for(Coord c : neighs){
			  if(simOut.containsKey(c)){
				  if(simGet.apply(c).getColor().equals(aTile.getColor()))
					  borderAdd.enqueue(simOut.remove(c));
				  else
					  onBorder = true;
			  }
		  }
		  simIn.put(aTile.getCoord(), aTile);
	  }
	  
	  return simIn.size();
  }
  
  /**
   * TODO
   * 
   * Returns the "best" WaterColor for the next move. 
   * 
   * "Best" WaterColor is determined by counting the number of Tile in inside
   * for each possible move. The move with the greatest number of flooded tiles
   * is then returned.
   */
  public WaterColor suggest() {
	WaterColor corner = get(Coord.ORIGIN).getColor();
	WaterColor best = corner;
	int bestCount = 0;
	int testCount = 0;
	WaterColor[] colors = WaterColor.values();
	for(WaterColor col : colors){
		if(col != corner){
			testCount = simFlood(col);
			if(testCount > bestCount){
				bestCount = testCount;
				best = col;
			}
		}
	}
	return best;
  }
  
  
  /**
   * Returns a random play choice other than the current origin color.
   * Used to run batch tests, as suggest heuristic won't finish running on
   * my laptop.
   */
  public WaterColor suggest2(){
	  WaterColor corner = get(Coord.ORIGIN).getColor();
	  WaterColor color = WaterColor.pickOneExcept(corner);
	  return color;
  }
  
  /**
   * Returns a string representation of this board. Tiles are given as their
   * color names, with those inside the flooded region written in uppercase.
   */ 
  public String toString() {
    StringBuilder ans = new StringBuilder();
    for (int y = 0; y < size; y++) {
      for (int x = 0; x < size; x++) {
        Coord curr = new Coord(x, y);
        WaterColor color = get(curr).getColor();
        ans.append(inside.containsKey(curr) ? color.toString().toUpperCase() : color);
        ans.append("\t");
      }
      ans.append("\n");
    }
    return ans.toString();
  }
  
  /**
   * Simple testing.
   */
  public static void main(String... args) {
    // Print out boards of size 1, 2, ..., 5
    int n = 5;
    WaterColor w;
    for (int size = 1; size <= n; size++) {
      Board someBoard = new Board(size);
      
      /**
       * Test suggest for consistency and no board change.
       */
      System.out.println("~Original~\n" + someBoard); // Original board
      w = someBoard.suggest();
      assert w == someBoard.suggest(); // consistent?
      System.out.println("~No Change?~\n" + someBoard); // No change?
      
      /**
       * Test flood for proper flooding.
       */
      someBoard.flood(w);
      System.out.println("~Flooding?~\n" + someBoard); // proper flooding?
      
      /**
       * Test another suggested move.
       */
      w = someBoard.suggest();
      assert w == someBoard.suggest();
      someBoard.flood(w);
      System.out.println("~Flooding?~\n" + someBoard);
      System.out.println("~~~END BOARD~~~\n\n");
    }
  }
}






