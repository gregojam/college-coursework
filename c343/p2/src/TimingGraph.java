import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.*;
import java.awt.font.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.List;

import javax.imageio.ImageIO;
import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 * Support class to display a graph of the runtime data.
 */

public class TimingGraph extends JPanel {
  private int numRuns;
  private List<List<Integer>> timings;

  /**
   * Constructs a panel on which we can paint a graph of the given timing data.
   */
  public TimingGraph(List<List<Integer>> timings) {
    assert timings.size() > 0;
    this.timings = timings;
    setBackground(Color.WHITE);
    setOpaque(true);
    numRuns = timings.get(0).size();
    showAndTell();
  }

  /**
   * Paints a graph of the timing data onto this panel.
   */
  public void paintComponent(Graphics g) {
    super.paintComponent(g);
    Graphics2D g2 = (Graphics2D) g;
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    int w = getWidth();
    int h = getHeight();
    // Draw y-axis
    g2.draw(new Line2D.Double(numRuns, numRuns, numRuns, h - numRuns));
    // Draw x-axis
    g2.draw(new Line2D.Double(numRuns, h - numRuns, w - numRuns, h - numRuns));
    // Draw labels
    Font font = g2.getFont();
    FontRenderContext frc = g2.getFontRenderContext();
    LineMetrics lm = font.getLineMetrics("0", frc);
    float sh = lm.getAscent() + lm.getDescent();
    // Draw label on the y-axis
    String s = "time";
    float sy = numRuns + ((h - 2 * numRuns) - s.length() * sh) / 2 + lm.getAscent();
    for (int i = 0; i < s.length(); i++) {
      String letter = String.valueOf(s.charAt(i));
      float sw = (float) font.getStringBounds(letter, frc).getWidth();
      float sx = (numRuns - sw) / 2;
      g2.drawString(letter, sx, sy);
      sy += sh;
    }
    // Draw label on the x-axis
    s = "board size";
    sy = h - numRuns + (numRuns - sh) / 2 + lm.getAscent();
    float sw = (float) font.getStringBounds(s, frc).getWidth();
    float sx = (w - sw) / 2;
    g2.drawString(s, sx, sy);
    // Draw lines
    double xInc = (double) (w - 2 * numRuns) / (timings.get(0).size() - 1);
    double scale = (double) (h - 2 * numRuns) / getMax();
    g2.setPaint(Color.GREEN.darker());
    g2.setPaint(Color.RED);
    for (List<Integer> run : timings) {
      for (int i = 0; i < run.size() - 1; i++) {
        double x1 = numRuns + i * xInc;
        double y1 = h - numRuns - scale * run.get(i);
        double x2 = numRuns + (i + 1) * xInc;
        double y2 = h - numRuns - scale * run.get(i + 1);
        g2.draw(new Line2D.Double(x1, y1, x2, y2));
      }
      // Mark data points with small circles
      for (int i = 0; i < run.size(); i++) {
        double x = numRuns + i * xInc;
        double y = h - numRuns - scale * run.get(i);
        g2.fill(new Ellipse2D.Double(x - 2, y - 2, 4, 4));
      }
      g2.setPaint(Color.LIGHT_GRAY);
    }
  }

  /**
   * Returns the largest value in the timing data.
   */
  private int getMax() {
    int max = Integer.MIN_VALUE;
    for (List<Integer> run : timings)
      for (int x : run)
        max = Math.max(x, max);
    return max;
  }

  /**
   * Frames the graph of the timing data (and displays it) and also writes it to
   * an image file.
   */
  private void showAndTell() {
    // Show
    JFrame frame = new JFrame();
    frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    frame.add(this);
    frame.setPreferredSize(new Dimension(400, 400));
    frame.setLocation(200, 200);
    frame.pack();
    frame.setVisible(true);    
    Dimension dim = frame.getSize();
    BufferedImage image = new BufferedImage(dim.width, dim.height, BufferedImage.TYPE_INT_ARGB);
    Graphics g = image.createGraphics();
    frame.paint(g);
    g.dispose();
    // Tell
    try {
      ImageIO.write(image, "png", new File(Constants.TEST_GRAPH));
    }
    catch (IOException e) {
      System.out.println("Unable to write graph to the file " + Constants.TEST_GRAPH);
    }
  }
}
