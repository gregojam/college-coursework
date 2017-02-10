/**
 * TODO
 * @author James Gregory
 */

public class Driver {
  
  private static int numCollisions;
  
  /**
   * TODO
   * 
   * Return the ColorTable associated with this image, assuming the color key space
   * is restricted to bitsPerChannel. Increment numCollisions after each increment.
   */
  public static ColorTable vectorize(Image image, int bitsPerChannel) {
	  ColorTable table = new ColorTable(7, bitsPerChannel, Constants.QUADRATIC, .49);
	  image = image.quantize(bitsPerChannel);
	  for (int x = 0; x < image.getWidth(); x++){
		    for (int y = 0; y < image.getHeight(); y++){
		    	table.increment(image.getColor(x, y));
		    	numCollisions = ColorTable.getNumCollisions();
		    }
	  }
    return table;
  }

  /**
   * TODO
   * 
   * Return the result of running Util.cosineSimilarity() on the vectorized images.
   * 
   * Note: If you compute the similarity of an image with itself, it should be close to 1.0.
   */
  public static double similarity(Image image1, Image image2, int bitsPerChannel) {
	  ColorTable a = vectorize(image1, bitsPerChannel);
	  ColorTable b = vectorize(image2, bitsPerChannel);
    return Util.cosineSimilarity(a, b);
  }

  /**
   * Uses the Painting images and all 8 bitsPerChannel values to compute and print 
   * out a table of collision counts.
   */
  public static void allPairsTest() {
    Painting[] paintings = Painting.values();
    int n = paintings.length;
    for (int y = 0; y < n; y++) {
      for (int x = y + 1; x < n; x++) {
        System.out.println(paintings[y].get().getName() + 
            " and " + 
            paintings[x].get().getName() + ":");
        for (int bitsPerChannel = 1; bitsPerChannel <= 8; bitsPerChannel++) {
          numCollisions = 0;
          System.out.println(String.format("   %d: %.2f %d", 
              bitsPerChannel,
              similarity(paintings[x].get(), paintings[y].get(), bitsPerChannel),
              numCollisions));
        }
        System.out.println();
      }
    }
    System.out.println(paintings[0].get().getName() + 
            " and " + 
            paintings[0].get().getName() + ":");
    numCollisions = 0;
    for(int bitsPerChannel = 1; bitsPerChannel <= 8; bitsPerChannel++){
	    System.out.println(String.format("   %d: %.2f %d", 
	        bitsPerChannel,
	        similarity(paintings[0].get(), paintings[0].get(), bitsPerChannel),
	        numCollisions));
    }
  }

  /**
   * Simple testing
   */  
  public static void main(String[] args) {
    System.out.println(Constants.TITLE);
    Image mona = Painting.MONA_LISA.get();
    Image starry = Painting.STARRY_NIGHT.get();
    Image christina = Painting.CHRISTINAS_WORLD.get();
    System.out.println("It looks like all three test images were successfully loaded.");
    System.out.println("mona's dimensions are " + 
        mona.getWidth() + " x " + mona.getHeight());
    System.out.println("starry's dimenstions are " + 
        starry.getWidth() + " x " + starry.getHeight());
    System.out.println("christina's dimensions are " + 
        christina.getWidth() + " x " + christina.getHeight());
    allPairsTest();
  }
}
