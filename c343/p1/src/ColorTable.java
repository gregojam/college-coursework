import java.awt.Color;

/**
 * @author James Gregory
 * 
 * A ColorTable represents a dictionary of frequency counts, keyed on Color.
 * It is a simplification of Map<Color, Integer>. The size of the key space
 * can be reduced by limiting each Color to a certain number of bits per channel.
 */

/**
 * TODO
 * 
 * Implement this class, including whatever data members you need and all of the
 * public methods below. You may create any number of private methods if you find
 * them to be helpful. Replace all TODO comments with appropriate javadoc style 
 * comments. Be sure to document all data fields and helper methods you define.
 */

public class ColorTable {
	
	/**
	 * @author James Gregory
	 *
	 *A Pair is an association of a color key code and a frequency of that color
	 */
	private class Pair{
		private int c;
		private long n;
		
		public Pair(int c, long n){
			this.c = c;
			this.n = n;
		}
		
		public int getColor(){
			return this.c;
		}
		
		public long getCount(){
			return this.n;
		}
		
		public void increment(){
			this.n++;
		}
	}
  /**
   * Counts the number of collisions during an operation.
   */
  private static int numCollisions = 0;
  
  /**
   * Array of color values
   */
  private Pair[] table;
  
  /**
   * Number of elements in array
   */
  private int tableCount;
  
  /**
   * Number of bits per channel
   */
  private int bitsPerChannel;
  
  /**
   * Selected collision handling strategy
   */
  private int collisionStrategy;
  
  /**
   * Rehash threshold
   */
  private double threshold;  

  /**
   * TODO
   * 
   * Constructs a color table with a starting capacity of initialCapacity. Keys in
   * the color key space are truncated to bitsPerChannel bits. The collision resolution
   * strategy is specified by passing either Constants.LINEAR or Constants.QUADRATIC for
   * the collisionStrategy parameter. The rehashThrehold specifies the maximum tolerable load 
   * factor before triggering a rehash.
   * 
   * @throws RuntimeException if initialCapacity is not in the range [1..Constants.MAX_CAPACITY]
   * @throws RuntimeException if bitsPerChannel is not in the range [1..8]
   * @throws RuntimeException if collisionStrategy is not one of Constants.LINEAR or Constants.QUADRATIC
   * @throws RuntimeException if rehashThreshold is not in the range (0.0..1.0] for a
   *                             linear strategy or (0.0..0.5) for a quadratic strategy
   */
  public ColorTable(int initialCapacity, int bitsPerChannel, int collisionStrategy, double rehashThreshold) { 
	  /**
	   * coll and exEnd are used in RuntimeException thrown when rehashThreshold is not appropriate
	   * for selected collisionStrategy
	   */
	  int coll;
	  char[] exEnd = {']', ')'};

	  if(initialCapacity < 1 || initialCapacity > Constants.MAX_CAPACITY)
		  throw new RuntimeException("initial capacity must be in range [1, " + Constants.MAX_CAPACITY + "]");

	  if(bitsPerChannel < 1 || bitsPerChannel > 8)
		  throw new RuntimeException("bits per channel must be in range [1, 8]");
	  
	  if(collisionStrategy != Constants.LINEAR && collisionStrategy != Constants.QUADRATIC)
		  throw new RuntimeException("collision strategy must be " + 
				  						Constants.LINEAR + " or " + Constants.QUADRATIC);
	  else if(collisionStrategy == Constants.LINEAR)
		  coll = 0;
	  else
		  coll = 1;
	  
	  if(collisionStrategy == Constants.LINEAR && (rehashThreshold < 0 || rehashThreshold > 1) ||
			  collisionStrategy == Constants.QUADRATIC && (rehashThreshold < 0 || rehashThreshold >= .5))
		  throw new RuntimeException("for collision strategy " + collisionStrategy + 
				  						",\nrehash threshold must be in range (0.0, " +
				  						(1 - coll * .5) + exEnd[coll]);
	  this.table = new Pair[initialCapacity];
	  this.tableCount = 0;
	  this.bitsPerChannel = bitsPerChannel;
	  this.collisionStrategy = collisionStrategy;
	  this.threshold = rehashThreshold;
  }
  
  /**
   * Returns the number of collisions that occurred during the most recent get or
   * put operation.
   */
  public static int getNumCollisions() {
    return numCollisions;
  }

  /**
   * TODO
   * 
   * Returns the number of bits per channel used by the colors in this table.
   */
  public int getBitsPerChannel() {
    return bitsPerChannel;
  }

  /**
   * TODO
   * 
   * Returns the frequency count associated with color. Note that colors not
   * explicitly represented in the table are assumed to be present with a
   * count of zero. Uses Util.pack() as the hash function.
   */
  public long get(Color color) {
	int code = Util.pack(color, bitsPerChannel);
	int i = code % table.length;
	int x = 1;
	while(getColorAt(i) != code){
		if(isEmptyAt(i))
			return 0;
		numCollisions++;
		if(collisionStrategy == Constants.LINEAR){
			i = ++i % table.length;
		}
		else
			i = (i + x * x++ - i) % table.length;
		if(i < 0) // i has overloaded. Give up search.
			return 0;
	}
	return getCountAt(i);
  }

  /**
   * TODO
   * 
   * Associates the count with the color in this table. Do nothing if count is less than
   * or equal to zero. Uses Util.pack() as the hash function.
   */
  public void put(Color color, long count) {
	  if(get(color) == 0){
		  while((double)(tableCount + 1) / table.length >= threshold)
			  rehash();
	  }
	  if(count > 0){
		  int code = Util.pack(color, bitsPerChannel);
		  int i = code % table.length;
		  int x = 1;
		  while(!isEmptyAt(i) && getColorAt(i) != code){
			  numCollisions++;
			  if(collisionStrategy == Constants.LINEAR)
				  i = ++i % table.length;
			  else
				  i = (i + x * x++ - i) % table.length;
		  }
		  if(getColorAt(i) != code)
			  tableCount++;
		  table[i] = new Pair(code, count);
	  }
  }

  /**
   * TODO
   * 
   * Increments the frequency count associated with color. Note that colors not
   * explicitly represented in the table are assumed to be present with a
   * count of zero.
   */
  public void increment(Color color) {
	  int code = Util.pack(color, bitsPerChannel);
	  int i = code % table.length;
	  int x = 1;
	  while(!isEmptyAt(i) && getColorAt(i) != code){
		  if(collisionStrategy == Constants.LINEAR)
			  i = ++i % table.length;
		  else
			  i = (i + x * x++) % table.length;
	  }
	  if(isEmptyAt(i))
		  put(color, 1);
	  else
		  table[i].increment();
  }

  /**
   * TODO
   * 
   * Returns the load factor for this table.
   */
  public double getLoadFactor() {
    return (double)tableCount / table.length;
  }

  /**
   * TODO
   * 
   * Returns the size of the internal array representing this table.
   */
  public int getCapacity() {
    return table.length;
  }

  /**
   * TODO
   * 
   * Returns the number of key/value associations in this table.
   */
  public int getSize() {
    return tableCount;
  }

  /**
   * TODO
   * 
   * Returns true iff this table is empty.
   */
  public boolean isEmpty() {
    return tableCount == 0;
  }

  /**
   * TODO
   * 
   * Increases the size of the array to the smallest prime greater than double the 
   * current size that is of the form 4j + 3, and then moves all the key/value 
   * associations into the new array. 
   * 
   * Hints: 
   * -- Make use of Util.isPrime().
   * -- Multiplying a positive integer n by 2 could result in a negative number,
   *    corresponding to integer overflow. You should detect this possibility and
   *    crop the new size to Constants.MAX_CAPACITY.
   * 
   * @throws RuntimeException if the table is already at maximum capacity.
   */
  
  private void rehash() {
	  if(table.length == Constants.MAX_CAPACITY)
		  throw new RuntimeException("table overflow");
	  
	  int n = 3; // 4 * 0 + 3
	  int j = 1;	  
	  while(n <= 2 * table.length || !Util.isPrime(n)){
		  if(n > Constants.MAX_CAPACITY){
			  n = Constants.MAX_CAPACITY;
			  break;
		  }
		  n = 4 * j + 3;
		  j++;
	  }
	  
	  Pair[] arr = new Pair[table.length];
	  j = 0;
	  for(int i = 0; i < table.length; i++){
		  if(j >= tableCount)
			  break;
		  else if(!isEmptyAt(i)){
			  arr[i] = table[i];
			  j++;
		  }
	  }
	  
	  table = new Pair[n];
	  tableCount = 0;
	  for(int i = 0; i < arr.length; i++){
		  if(j == 0)
			  break;
		  else if(arr[i] != null){
			  put(Util.unpack(arr[i].getColor(), bitsPerChannel), arr[i].getCount());
			  j--;
		  }
	  }
  }

  /**
   * TODO
   * 
   * Returns an Iterator that marches through each color in the key color space and
   * returns the sequence of frequency counts.
   */
  public Iterator iterator() {
	  return new Iterator(){
		  long curr = 0, end = table.length - 1;
		  public boolean hasNext(){
			  return curr <= end;
		  }
		  public long next(){
			  return getCountAt((int)curr++);
		  }
	  };
  }

  /**
   * TODO
   * 
   * Returns a String representation of this table.
   */
  public String toString() {
    String str = "{";
    for(int i = 0; i < table.length; i++){
    	if(!isEmptyAt(i))
    		str += i + ":(" + getColorAt(i) + ", " + getCountAt(i) + "), ";
    }
    if(str == "{")
    	return("{}");
    return(str.substring(0, str.length() - 2) + "}");
  }

  /**
   * TODO
   * 
   * Returns the count in the table at index i in the array representing the table.
   * The sole purpose of this function is to aid in writing the unit tests.
   */
  public long getCountAt(int i) {
	  if(isEmptyAt(i))
		  return 0;
    return table[i].getCount();
  }
  
  /**
   * 
   * Returns the color code in the table at index i in the array representing the table.
   */
  public int getColorAt(int i){
	  if(isEmptyAt(i))
		  return -1;
	  return table[i].getColor();
  }
  
  /**
   * Return a boolean of whether or not an index i in the array representing the table is empty.
   */
  public boolean isEmptyAt(int i){
	  return i > table.length - 1 || table[i] == null;
  }
  
  /**
   * Simple testing.
   */
  public static void main(String[] args) {
    ColorTable table = new ColorTable(3, 6, Constants.QUADRATIC, .49);
    int[] data = new int[] { 32960, 4293315, 99011, 296390 };
    
    /**
     * Test printing that prints current ColorTable (omitting null elements),
     * followed by "->", then the table capacity, and finally the table size.
     * 
     * Expected results:
     * 		{2:(2096, 1)} -> 3, 1
     * 		{3:(2096, 1), 5:(67632, 1)} -> 7, 2
     * 		{3:(2096, 2), 5:(67632, 1)} -> 7, 2
     * 		{3:(2096, 1), 5:(67632, 1), 6:(6257, 1)} -> 7, 3
     */
    for (int i = 0; i < data.length; i++){
      table.increment(new Color(data[i]));
      System.out.println(table + " -> " + table.getCapacity() + ", " + table.getSize());
    }
    
    assert(table.getCapacity() == 7);
    assert(table.getSize() == 3);
    assert(table.get(new Color(data[0])) == 2);
    assert(table.get(new Color(data[1])) == 1);
    assert(table.get(new Color(data[2])) == 2);
    assert(table.get(new Color(data[3])) == 1);
    assert(table.get(new Color(0)) == 0);
    assert(table.getCountAt(3) == 2);
    assert(table.getCountAt(1) == 0);
    assert(table.getColorAt(5) == 67632);
    assert(table.getColorAt(4) == -1);
    assert(table.isEmptyAt(2));
    assert(!table.isEmptyAt(6));
  }
}
