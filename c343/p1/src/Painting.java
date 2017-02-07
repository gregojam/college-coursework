/**
 * Just a few specific paintings (good for testing purposes).
 */

public enum Painting {
  MONA_LISA("davinci.jpg"),
  CHRISTINAS_WORLD("wyeth.jpg"),
  STARRY_NIGHT("vangogh.jpg"),
  BLUE_DANCERS("degas.jpg");
  
  private Image image;

  private Painting(String filename) {
    this.image = new Image(Constants.IMAGE_DIR + "/" + filename);
  }
  
  /**
   * Returns the Image object associated with this painting.
   */
  public Image get() {
    return image;
  }
}


