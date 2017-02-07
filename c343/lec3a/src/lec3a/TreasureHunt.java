package lec3a;

/**
 * Given a treasure map and a starting position, follow the clues on the
 * map to a location containing treasure and dig it up.
 * 
 *  @author James Gregory
 */
public class TreasureHunt {
  
  /**
   * Follows the clues in the map and returns the buried treasure.
   * 
   * @param start The starting point for the hunt.
   * @param map An array representing the treasure map. The array
   * elements are of type Entry, and each Entry is either a Clue or 
   * the actual Treasure. 
   * 
   * @return What is found when you dig for treasure after following
   * the clues in the map.
   */

  public static String hunt(int start, Entry[] map) {
    Entry ent = new Clue(0); //set up dummy Clue to get into  loop
    while(ent.hasNext()){
    	start += ent.next();
    	if(start < 0 || start >= map.length)
    		throw new ArrrException();
    	ent = map[start];
    }
    return ent.dig();
  }
  
  public static void main(String... args) {
    Entry[] map;
    String loot;
    
    map = new Entry[] {
        new Treasure("gold"),
        new Clue(1),
        new Clue(1),
        new Treasure("silver"),
    };
    loot = hunt(0, map);
    assert loot.equals("gold");

    map = new Entry[] {
        new Treasure("gold"),
        new Clue(2),
        new Clue(1),
        new Clue(1),
        new Treasure("silver"),
    };
    loot = hunt(1, map);
    assert loot.equals("silver");

    assert hunt(0, new Entry[] {
        new Clue(3),
        new Treasure("silver"),
        new Treasure("gold"),
        new Clue(2),
        new Clue(2),
        new Treasure("diamonds"),
    }).equals("diamonds");

    assert hunt(1, new Entry[] {
        new Clue(7),
        new Clue(3),
        new Clue(6),
        new Treasure("rubies"),
        new Clue(-2),
        new Treasure("gold"),
        new Clue(1),
        new Treasure("silver"),
        new Clue(-3),
    }).equals("gold");

    try {
      hunt(-1, new Entry[] {
          new Clue(3),
          new Clue(-1),
          new Treasure("gold"),
          new Clue(2),
          new Treasure("silver"),
          new Clue(-3),
      });
    }
    catch (ArrrException e) {
      System.out.println(e);
    }

    try {
      hunt(0, new Entry[] {
          new Clue(2),
          new Treasure("bigmac"),
          new Clue(3),
          new Clue(2),
          new Clue(2),
          new Clue(-1),
          new Clue(3),
          new Clue(3),
          new Treasure("whopper"),
          new Clue(3),
          new Treasure("fries"),
          new Treasure("coke"),
      });
    }
    catch (ArrrException e) {
      System.out.println(e);
    }
    
    assert hunt(0, new Entry[]{
    		new Clue(9),
    		new Clue(7),
    		new Clue(5),
    		new Clue(3),
    		new Clue(1),
    		new Treasure("Awesome Sauce"),
    		new Clue(-2),
    		new Clue(-4),
    		new Clue(-6),
    		new Clue(-8)
    }).equals("Awesome Sauce");
  }
}

abstract class Entry {
  abstract int next();
  abstract boolean hasNext();
  abstract String dig();
}

class Clue extends Entry {
  private int k;
  
  public Clue(int k) {
    this.k = k;
  }
  
  public int next() {
    return k;
  }
  
  public boolean hasNext() {
    return true;
  }
  
  public String dig() {
    throw new ArrrException();
  }
}

class Treasure extends Entry {
  private String treasure;
  
  public Treasure(String treasure) {
    this.treasure = treasure;
  }
  
  public int next() {
    throw new ArrrException();
  }
  
  public boolean hasNext() {
    return false;
  }
  
  public String dig() {
    return treasure;
  }
}

class ArrrException extends RuntimeException {
  public ArrrException() {
    super("Arrr!");
  }
}
