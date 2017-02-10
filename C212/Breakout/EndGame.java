////////////////////
//
//C212 Spring 2016
//Lab 8
//
//Author: James Gregory
//Last Modified: 4/22/16
//
////////////////////

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import javax.swing.SwingConstants;

import java.awt.Container;
import java.awt.Font;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

class EndGame extends JFrame{
  
  JLabel message;
  JPanel panel;
  
  public EndGame(int score){
    super("Game Over");
    
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    
    message = new JLabel("Final Score: " + score);
    message.setFont(new Font("Verdana", 1, 20));
    message.setForeground(Color.white);
    message.setHorizontalAlignment(SwingConstants.CENTER);

    panel = new JPanel();
    panel.setPreferredSize(new Dimension(400, 100));
    panel.setBackground(Color.gray);
    panel.setLayout(new BorderLayout());
    panel.add(message);
    
    Container surface = getContentPane();
    surface.add(panel);
    pack();
    setLocationRelativeTo(null);
    setAlwaysOnTop(true);
    setVisible(true);
    setAlwaysOnTop(false);
  }
  
//  public static void main(String[] args){}
}