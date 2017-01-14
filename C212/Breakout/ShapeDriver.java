////////////////////////////////////////////////////////////////////////////////////
//
//  C212 Spring 16
//
//  Homework 6 Template
//  Due: Friday 3/11 11:59 pm
//  @Author  Earl Dean
//
// Modified By: James Gregory
// Last Modified: 4/22/16
//
///////////////////////////////////////////////////////////////////////////////////
 
// These are the imports I used 
import java.util.List;
import java.util.ArrayList;
import java.util.Random;

import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.Timer;

import java.awt.Rectangle;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Font;
import java.awt.BorderLayout;
import java.awt.GridLayout;
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
    public final int FRAME_WIDTH = 1001;
    public final int FRAME_HEIGHT = 600;

    private Random random;
    private ArrayList<Rectangle2> blocks;
    private Rectangle2 paddle;
    private Circle ball;
    private Timer timer;
    private Timer endTimer;
    private Rectangle union;
    private int lives;
    private int score;
    private EndGame end;
    
    JPanel wordStuff;    
    JLabel scoreLabel;
    JLabel livesLabel;

    public ShapeDriver() {
        super(); 
        
        JLabel howToMove = new JLabel("arrow keys to move paddle");
        howToMove.setFont(new Font("Verdana", 1, 20));
        howToMove.setForeground(Color.white);
        howToMove.setHorizontalAlignment(JLabel.CENTER);
        
        JLabel howToStart = new JLabel("spacebar to launch ball");
        howToStart.setFont(new Font("Verdana", 1, 20));
        howToStart.setForeground(Color.white);
        howToStart.setHorizontalAlignment(JLabel.CENTER);
        
        lives = 3;
        livesLabel = new JLabel("Lives: " + lives);
        livesLabel.setFont(new Font("Verdana", 1, 20));
        livesLabel.setForeground(Color.white);
        livesLabel.setHorizontalAlignment(JLabel.CENTER);
        
        score = 0;
        scoreLabel = new JLabel("Score: " + score);
        scoreLabel.setFont(new Font("Verdana", 1, 20));
        scoreLabel.setForeground(Color.white);
        scoreLabel.setHorizontalAlignment(JLabel.CENTER);
        
        JPanel instructions = new JPanel();
        instructions.setBackground(Color.black);
        instructions.setPreferredSize(new Dimension(FRAME_WIDTH, 50));
        instructions.setLayout(new BorderLayout());
        instructions.add(howToMove, BorderLayout.NORTH);
        instructions.add(howToStart, BorderLayout.SOUTH);
        
        wordStuff = new JPanel();
        wordStuff.setBackground(Color.black);
        wordStuff.setPreferredSize(new Dimension(FRAME_WIDTH, 50));
        wordStuff.setLayout(new GridLayout());
        wordStuff.add(livesLabel);
        wordStuff.add(instructions);
        wordStuff.add(scoreLabel);
                             
        random = new Random();
        
        union = new Rectangle();
        
        blocks = new ArrayList<>();
        
        newBlocks();
                     
        newPaddle();
        
        newBall();
        
        addKeyListener(this);
        setFocusable(true);
        setPreferredSize(new Dimension(FRAME_WIDTH, FRAME_HEIGHT));
        setBackground(Color.gray);
        setLayout(new BorderLayout());
        add(wordStuff, BorderLayout.NORTH);
        
        timer = new Timer(16, this);
    }

    @Override
    public void paintComponent(Graphics g) {
      super.paintComponent(g);
      
      paddle.draw(g);
      ball.draw(g);
      
      for(Rectangle2 block : blocks){
        block.draw(g);
      }
    }
    
    @Override
    public void keyPressed(KeyEvent e) {      
      switch (e.getKeyCode()){
        case KeyEvent.VK_LEFT:{
          if(timer.isRunning()){
            if(paddle.getX() > 15)
              paddle.moveLoc(-15, 0);
            else
              paddle.setLocation(0, paddle.getY());
            repaint();
          }
          break;
        }
        case KeyEvent.VK_RIGHT:{
          if(timer.isRunning()){
            if(paddle.getX() < FRAME_WIDTH - paddle.getWidth() - 15)
              paddle.moveLoc(15, 0);
            else
              paddle.setLocation(FRAME_WIDTH - paddle.getWidth(), paddle.getY());
            repaint();
          }
          break;
        }
        case KeyEvent.VK_SPACE:
          timer.start();
      }
    }

    @Override
    public void keyReleased(KeyEvent e) { }
    
    @Override
    public void keyTyped(KeyEvent e) { }
    
    public void actionPerformed(ActionEvent e){
      ball.moveLoc(ball.getMoveX(), ball.getMoveY());
      if(ball.getX() <= 0){
        ball.setX(0);
        ball.setMoveX(-ball.getMoveX());
      }
      if(ball.getX() >= FRAME_WIDTH - ball.getRect().getWidth()){
        ball.setX(FRAME_WIDTH - ball.getRadius() * 2);
        ball.setMoveX(-ball.getMoveX());
      }
      if(ball.getY() <= 50){
        ball.setY(50);
        ball.setMoveY(-ball.getMoveY());
      }
      if(ball.getY() >= FRAME_HEIGHT - ball.getRect().getHeight()){
        livesLabel.setText("Lives: " + --lives);
        timer.stop();
        if(lives > 0){
          newPaddle();
          newBall();
        }
        else{
          timer.stop();
          new EndGame(score);
        }
      }
      
      if(ball.getRect().intersects(paddle.getRect())){
        union = ball.getRect().union(paddle.getRect());
        if(union.getWidth() - paddle.getWidth() > union.getHeight() - paddle.getHeight()){
          if(ball.getX() < paddle.getX())
            ball.setX(paddle.getX() - ball.getRadius());
          else
            ball.setX(paddle.getX() + paddle.getWidth());
          ball.setMoveX(-ball.getMoveX());
        }
        else{
          int hitWhere = (ball.getX() + ball.getRadius()
                            - (paddle.getX() + paddle.getWidth() / 2)) / 10;
          ball.setY(paddle.getY() - ball.getRadius() * 2);
          ball.setMoveY(-ball.getMoveY());
          ball.setMoveX(ball.getMoveX() + hitWhere);
        }
      }
      
      for(Rectangle2 block : blocks){
        if(ball.getRect().intersects(block.getRect())){
          union = ball.getRect().union(block.getRect());
          if(union.getWidth() - block.getWidth() > union.getHeight() - block.getHeight()){
            if(ball.getX() < block.getX())
              ball.setX(block.getX() - ball.getRadius());
            else
              ball.setX(block.getX() + block.getWidth());
            ball.setMoveX(-ball.getMoveX());
          }
          else{
            if(ball.getY() < block.getY())
              ball.setY(block.getY() - ball.getRadius() * 2);
            else
              ball.setY(block.getY() + block.getHeight());
            ball.setMoveY(-ball.getMoveY());
          }
          blocks.remove(block);
          scoreLabel.setText("Score: " + ++score);
          if(blocks.isEmpty()){
            newBlocks();
            newPaddle();
            newBall();
          }
          break;
        }
      }
      repaint();
    }
    
    private void newBlocks(){
      ArrayList<Color> colors = new ArrayList<>();
      colors.add(Color.red);
      colors.add(Color.blue);
      colors.add(Color.green);
      colors.add(Color.yellow);
      
      int blockWidth = FRAME_WIDTH / 14 - 2;
      int blockHeight = 20;
      for(int i = 0; i < 8; i++){
        for(int j = 0; j < 7; j++){
          blocks.add(new Rectangle2(colors.get(i % colors.size()),
                                    colors.get(i % colors.size()),
                                    FRAME_WIDTH / 2 + 1 + j * (blockWidth + 2),
                                    50 + i * (blockHeight + 3),
                                    blockWidth,
                                    blockHeight));
          blocks.add(new Rectangle2(colors.get(i % colors.size()),
                                    colors.get(i % colors.size()),
                                    FRAME_WIDTH / 2 - 1 - blockWidth - j * (blockWidth + 2),
                                    50 + i * (blockHeight + 3),
                                    blockWidth,
                                    blockHeight));
        }
      }
    }
    
    private void newBall(){
      ball = (new Circle(Color.WHITE,
                         Color.WHITE,
                         FRAME_WIDTH / 2 - 5,
                         FRAME_HEIGHT - 100,
                         5));
    }
    
    private void newPaddle(){
      paddle = new Rectangle2(FRAME_WIDTH / 2 - 50,
                              FRAME_HEIGHT - 50,
                              100, 10);
    }

    // test client
//    public static void main(String[] args) {
//      new ShapeDriver();
//    }
}