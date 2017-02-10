import java.awt.image.BufferedImage;
import java.awt.Color;
import java.awt.Graphics;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

/**
 * This class handles the loading of images from files, and it supports accessing the color of 
 * a specific pixel.
 */

public class Image {
	
	private BufferedImage image;
	private int width, height;
	private String name;
	
	/**
	 * Load an external image from the file system. Exits on error.
	 */
	public Image(String filename) {
		try {
		  String[] words = filename.split("/");
		  int last = words.length - 1;
		  name = words[last].substring(0, words[last].indexOf("."));
			image = ImageIO.read(new File(filename));
			width = image.getWidth();
			height = image.getHeight();
		} 
		catch (IOException e) {
			System.out.println("Image could not be read: " + filename);
			System.exit(1);
		}
	}
	
	/**
	 * Construct a copy of source in this image.
	 */
	public Image(Image source) {
    this.width = source.width;
    this.height = source.height;
    this.name = source.name;
    this.image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
    Graphics g = this.image.getGraphics();
    g.drawImage(source.image, 0, 0, null);
    g.dispose();
	}
	
	/**
	 * Return a quantized version of this image where the given number of lower-order bits 
	 * of each color component are masked to zeroes.
	 */
	public Image quantize(int bitsPerChannel) {
	  Image copy = new Image(this);
	  for (int x = 0; x < getWidth(); x++)
	    for (int y = 0; y < getHeight(); y++)
	      copy.setColor(x, y, Util.unpack(Util.pack(getColor(x, y), bitsPerChannel), bitsPerChannel));
	  return copy;
	}
	
	/**
	 * Returns the name associated with this image.
	 */
	public String getName() {
	  return name;
	}

  /**
   * Returns the width (in pixels) of this image.
   */
	public int getWidth() { 
	  return width; 
	}
	
  /**
   * Returns the height (in pixels) of this image.
   */
	public int getHeight() { 
	  return height; 
	}
	
  /**
   * Returns the color in this image at coordinate (x, y).
   */	
	public Color getColor(int x, int y) {
	  try {
	    return new Color(image.getRGB(x, y));
	  }
	  catch (RuntimeException e) {
	    return Color.WHITE;
	  }
	}
	
	/**
	 * Sets the pixel at location (x, y) to the specified color.
	 */
	public void setColor(int x, int y, Color color) {
	  image.setRGB(x, y, color.getRGB());
	}
	
	/**
	 * Uses the given graphics context to draw this image on the associated
	 * component.
	 */
	public void draw(Graphics gr) {
	  gr.drawImage(image, 0, 0, null);
	}

	/**
	 * Simple testing.
	 */
	public static void main(String[] args) {
		Image image = new Image(Constants.IMAGE_DIR + "/davinci.jpg");
		System.out.println("corner color: " + image.getColor(0, 0));
	}

}
