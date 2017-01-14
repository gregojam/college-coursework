////////////////////////////////////////////////////////////////////////////////////
//
//  C212 Spring 16
//
//  Homework 6 Template
//  Due: Friday 3/11 11:59 pm
//  @Author  Earl Dean
//
// Modified By: James Gregory
// Last Modified: 4/11/16
//
///////////////////////////////////////////////////////////////////////////////////
 
// These are the imports I used 
import java.util.List;
import java.util.ArrayList;
import java.util.Random;

import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.SwingConstants;
import javax.swing.Timer;

//import java.awt.Point;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Font;
import java.awt.BorderLayout;
import java.awt.event.KeyListener;
import java.awt.event.KeyEvent;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/*
 * Driver program for randam shape generator app
 */
public class ShapeDriver extends JPanel
  implements KeyListener, ActionListener{

    // Panel constants
    public final int FRAME_WIDTH = 600;
    public final int FRAME_HEIGHT = 600;

    private Random random;
    private ArrayList<Shape> daShapes;
    private Timer timer;

    public ShapeDriver() {
        super(); 
        
        JLabel instructions = new JLabel("(C)ircle, (R)ectangle, (S)quare");
        instructions.setFont(new Font("Verdana", 1, 20));
        instructions.setHorizontalAlignment(SwingConstants.CENTER);
                             
        random = new Random();
        daShapes = new ArrayList<>();
        
        addKeyListener(this);
        setFocusable(true);
        setPreferredSize(new Dimension(FRAME_WIDTH, FRAME_HEIGHT));
        setBackground(Color.gray);
        setLayout(new BorderLayout());
        add(instructions);
        
        timer = new Timer(16, this);
        timer.start();
    }

    @Override
    public void paintComponent(Graphics g) {
      super.paintComponent(g);
      
      for(Shape shape : daShapes){
        shape.draw(g);
      }
    }
    
    @Override
    public void keyPressed(KeyEvent e) {
      Color bColor;
      Color fColor;
      int x = 0;
      int y = 0;
      int w = 0;
      int h = 0;
      
      switch (e.getKeyCode()){
        case KeyEvent.VK_C:
          bColor = new Color(random.nextFloat(), random.nextFloat(), random.nextFloat());
          fColor = new Color(random.nextFloat(), random.nextFloat(), random.nextFloat());
          
          int radius = random.nextInt(25);
          x = random.nextInt(FRAME_WIDTH - 1 - 2 * radius);
          y = random.nextInt(FRAME_HEIGHT - 1 - 2 * radius);
          Circle c = new Circle(fColor, bColor, x, y, radius);
          daShapes.add(c);
          repaint();
          break;
        case KeyEvent.VK_R:
          bColor = new Color(random.nextFloat(), random.nextFloat(), random.nextFloat());
          fColor = new Color(random.nextFloat(), random.nextFloat(), random.nextFloat());
          
          w = random.nextInt(49) + 1;
          h = random.nextInt(49) + 1;
          x = random.nextInt(FRAME_WIDTH - 1 - w) + 1;
          y = random.nextInt(FRAME_HEIGHT - 1 - h) + 1;
          
          Rectangle2 r = new Rectangle2(fColor, bColor, x, y, w, h);
          daShapes.add(r);
          repaint();
          break;
        case KeyEvent.VK_S:
          bColor = new Color(random.nextFloat(), random.nextFloat(), random.nextFloat());
          fColor = new Color(random.nextFloat(), random.nextFloat(), random.nextFloat());
          
          w = random.nextInt(49) + 1;
          x = random.nextInt(FRAME_WIDTH - 1 - w) + 1;
          y = random.nextInt(FRAME_HEIGHT - 1 - w) + 1;
          
          Square s = new Square(fColor, bColor, x, y, w);          
          daShapes.add(s);
          repaint();
      }
    }

    // do not neet to do anything with this method from KeyListener
    // but must have since this class implements KeyListiner 
    @Override
    public void keyReleased(KeyEvent e) { }
    
    // do not neet to do anything with this method from KeyListener
    // but must have since this class implements KeyListiner 
    @Override
    public void keyTyped(KeyEvent e) { }
    
    public void actionPerformed(ActionEvent e){
      Color temp = Color.white;
      int ndex = 0;
      for(Shape shape : daShapes){
        shape.moveLoc(shape.getMoveX(), shape.getMoveY());
        if(shape.getX() <= 0 ||
           shape.getX() >= FRAME_WIDTH - shape.getRect().getWidth())
          shape.setMoveX(-shape.getMoveX());
        if(shape.getY() <= 0 ||
           shape.getY() >= FRAME_HEIGHT - shape.getRect().getHeight())
          shape.setMoveY(-shape.getMoveY());
        ndex = daShapes.indexOf(shape);
        for(Shape other : daShapes.subList(ndex, daShapes.size())){
          if(shape.getRect().intersects(other.getRect())){
            shape.setMoveX(-shape.getMoveX());
            shape.setMoveY(-shape.getMoveY());
            other.setMoveX(-other.getMoveX());
            other.setMoveY(-other.getMoveY());
            temp = shape.getFillColor();
            shape.setFillColor(other.getFillColor());
            other.setFillColor(temp);
          }
        }
      }
      repaint();
    }

    // test client
    public static void main(String[] args) {
      new ShapeDriver();
    }
}