import java.io.File;
import java.awt.Component;
import java.awt.GridLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Timer;
import java.util.TimerTask;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

public class GUI extends JFrame {
  // Lookup table for previously computed similarities.
  private Map<Integer, Double> simCache = new HashMap<>();
  // Images to compare.
  private Image leftImage, rightImage;
  // Display panels for images.
  private ArrayList<ImagePanel> panels = new ArrayList<>();
  // Indicative of whether or not the xray is turned on (initially false).
  private boolean xrayEffect;

  static {
    try {
      UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());
    }
    catch (Exception e) {
      e.printStackTrace(System.out);
    }
  }

  // bitSelector is the interface element for selecting desired number of bits per channel.
  JSlider bitSelector = new JSlider(SwingConstants.HORIZONTAL, 1, 8, 8) {{
    setBackground(Color.WHITE);
    setPaintTicks(true);
    setPaintLabels(true);
    setSnapToTicks(true);
    setMajorTickSpacing(1);
    setLabelTable(createStandardLabels(1));
    setPreferredSize(new Dimension(200, 60));
    setMaximumSize(new Dimension(200, 60));
    setToolTipText("Drop lower-order bits from RGB values");
  }};

  // colorBox is the interface element for displaying the most recently selected color
  // from one of the images.
  JLabel colorBox = new JLabel("00 00 00") {{
    setOpaque(true);
    setHorizontalAlignment(SwingConstants.CENTER);
    setBackground(Color.BLACK);
    setToolTipText("Click on an image pixel to change the current color");
  }
  public void setBackground(Color color) {
    super.setBackground(color);
    setForeground(getContrastColor(color));
    String text = String.format("%06X", color.getRGB());
    text = text.substring(2, 4) + " " + text.substring(4, 6) + " " + text.substring(6, 8);
    setText(text);
  }};

  /**
   * Constructs a window to display two images and compare them for similarity after
   * quantization.
   */
  public GUI(Image leftImage, Image rightImage) {
    this.leftImage = leftImage;
    this.rightImage = rightImage;
    setTitle(Constants.TITLE + " (" + leftImage.getName() + " and " + rightImage.getName() + ")");
    setBackground(Color.WHITE);

    JPanel main = new JPanel() {{
      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS)); 
      setBackground(Color.WHITE);
    }};

    // sideBySide is the interface element for displaying the two images side by side.
    ImagePanel A = new ImagePanel(leftImage), B = new ImagePanel(rightImage);
    JPanel sideBySide = new JPanel() {{
      setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
      setBackground(Color.WHITE);
      setOpaque(true);
      add(A);
      add(Box.createRigidArea(new Dimension(10, 0))); // put some space between the images
      add(B);
    }};

    // controls is the interface element holding the control panel.
    JPanel controls = new JPanel() {{
      setOpaque(true);
      setBackground(Color.WHITE);
      setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
    }};
    JLabel bitLabel = new JLabel(bitSelector.getValue() + " bits Per Channel");

    JLabel simLabel = new JLabel(String.format("Similarity: %1.2f", similarity()));
    simLabel.setToolTipText("Cosine similarity of the two images (max 1.0)");
    JButton newWindow = new JButton("New");
    JButton close = new JButton("Close");

    controls.add(Box.createRigidArea(new Dimension(30, 0)));
    controls.add(new JPanel() {{
      setPreferredSize(new Dimension(100, 60));
      setMaximumSize(new Dimension(100, 60));
      setMinimumSize(new Dimension(100, 60));
      setLayout(new GridLayout(0, 1));
      add(colorBox);
    }});
    controls.add(Box.createRigidArea(new Dimension(30, 0)));
    controls.add(new JPanel() {{
      setBackground(Color.WHITE);
      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
      add(bitLabel);
      bitLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
      add(bitSelector);
      bitSelector.setAlignmentX(Component.CENTER_ALIGNMENT);
    }});
    controls.add(Box.createRigidArea(new Dimension(30, 0)));
    controls.add(simLabel);
    controls.add(Box.createRigidArea(new Dimension(30, 0)));
    controls.add(newWindow);
    controls.add(Box.createRigidArea(new Dimension(30, 0)));
    controls.add(close);
    controls.add(Box.createRigidArea(new Dimension(30, 0)));

    bitSelector.addChangeListener(e -> {
      sideBySide.repaint();
      bitLabel.setText(bitSelector.getValue() + " bits Per Channel");
      simLabel.setText(String.format("Similarity: %1.2f", similarity()));
    });

    newWindow.addActionListener(e -> {
      GUI.main(null);
    });

    close.addActionListener(e -> dispose());

    main.add(sideBySide);
    main.add(Box.createRigidArea(new Dimension(0, 30)));;
    main.add(controls);
    main.add(Box.createRigidArea(new Dimension(0, 30)));;
    setContentPane(main);
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    pack();
    setVisible(true);
  }

  /**
   * Returns the similarity of the two images using however many bits per channel
   * appear on bitSelector. Results are memoized to speed the process.
   */
  private double similarity() {
    int bitsPerChannel = bitSelector.getValue();
    if (simCache.containsKey(bitsPerChannel))
      return simCache.get(bitsPerChannel);
    double sim = Driver.similarity(leftImage, rightImage, bitsPerChannel);
    simCache.put(bitsPerChannel, sim); // Jot down for later.
    return sim;
  }

  /**
   * An ImagePanel represents one panel displaying one image. The image is quantized depending
   * on the bitSelector value. This component listens for mouse events, and updates the 
   * colorBox control accordingly.
   */
  class ImagePanel extends JPanel {
    // The actual image.
    private Image image;   
    // The quantized image being displayed.
    private Image quantImage;
    // A timer to trigger the xray effect during a long press.
    private Timer timer; 

    public ImagePanel(Image image) {
      // Effects on one image affect both, so save all constructed panels in one place.
      panels.add(this);  
      setBackground(Color.WHITE);
      this.image = image;
      setOpaque(true);
      setPreferredSize(new Dimension(image.getWidth(), image.getHeight()));

      // Clicks change the color in the colorBox
      // Long clicks (press > hold > release) turn on the xray
      addMouseListener(new MouseAdapter() {
        public void mousePressed(MouseEvent e) {
          Color color = quantImage.getColor(e.getX(), e.getY());
          colorBox.setBackground(color);
          timer = new Timer();
          // Schedule the xray effect to turn on 100ms after the intial press.
          timer.schedule(new TimerTask() {
            public void run() {
              xrayEffect = true;
              // Xray both panels, not just the this one.
              for (ImagePanel panel : panels)
                panel.repaint();
            }
          }, 100);
        }       
        public void mouseReleased(MouseEvent e) {
          timer.cancel();
          if (xrayEffect) {
            xrayEffect = false;
            for (ImagePanel panel : panels)
              panel.repaint();
          }
        }
      });
    }
    
    public void paintComponent(Graphics g) {
      super.paintComponent(g);
      quantImage = image.quantize(bitSelector.getValue());
      if (xrayEffect) 
        xray(colorBox.getBackground(), quantImage).draw(g); 
      else
        quantImage.draw(g);   
    }
  }

  /**
   * Returns a color that contrasts visually with the given color.
   */
  public static Color getContrastColor(Color color) {
    double y = (299 * color.getRed() + 587 * color.getGreen() + 114 * color.getBlue()) / 1000;
    return y >= 128 ? Color.BLACK : Color.WHITE;
  }

  /**
   * Returns a black and white version of the given image, where pixels matching
   * color are white.
   */
  private static Image xray(Color color, Image image) {
    Image copy = new Image(image);
    for (int x = 0; x < copy.getWidth(); x++)
      for (int y = 0; y < copy.getHeight(); y++)
        copy.setColor(x, y, copy.getColor(x, y).equals(color) ? Color.WHITE : Color.BLACK);
    return copy;
  }

  /**
   * Fires up the GUI with two randomly selected images.
   */
  public static void main(String[] args) {
    // Load all the paintings in the image directory.
    List<Image> paintings = new ArrayList<>();
    for (File file : new File(Constants.IMAGE_DIR).listFiles())
      paintings.add(new Image(file.getPath()));
    int n = paintings.size();
    // Choose two different images, at random and with equal probability.
    Random rand = new Random();
    int i = rand.nextInt(n), j = rand.nextInt(n - 1);
    if (j >= i) 
      j++;
    Image pic1 = paintings.get(i), pic2 = paintings.get(j);
    SwingUtilities.invokeLater(() -> new GUI(pic1, pic2));    
  }
}
