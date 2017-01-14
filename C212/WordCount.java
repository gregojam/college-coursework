//////////////////////////////
//
//C212 Spring 2016
//Lab 13
//
//Author: James Gregory
//Last Modified: 4/18/16
//
//////////////////////////////

import java.util.HashMap;
import java.util.Scanner;
import java.util.Random;

import java.io.File;
import java.io.FileNotFoundException;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Color;

class WordCount extends JFrame{
  
  private JPanel panel;
  private JTextArea text;
  private HashMap<String, Integer> words;
  private String[] line;
  private Random random;
  
  public WordCount(){
    super("Word Count");
    
    words = new HashMap<>();
    
    random = new Random();
    
    text = new JTextArea("");
    text.setForeground(Color.BLACK);
    text.setLineWrap(true);
    text.setWrapStyleWord(true);
    text.setEditable(false);
    
    // Button to select file
    JButton choiceButton = new JButton("File Selector");
    choiceButton.addActionListener(new ActionListener(){
      public void actionPerformed(ActionEvent e){
        words.clear();
        JFileChooser chooser = new JFileChooser();
        FileNameExtensionFilter filter = new FileNameExtensionFilter("Text files", "txt");
        chooser.setFileFilter(filter);
        int choice = chooser.showOpenDialog(null);
        if(choice == JFileChooser.APPROVE_OPTION){
          reader(chooser.getSelectedFile());
        }
      }
    });
    
    // Button to clear text
    JButton clearText = new JButton("Clear");
    clearText.addActionListener(new ActionListener(){
      public void actionPerformed(ActionEvent e){
        text.setText("");
      }
    });
    
    // Button to change background color
    JButton colorButton = new JButton("Change Color");
    colorButton.addActionListener(new ActionListener(){
      public void actionPerformed(ActionEvent e){
        text.setBackground(new Color(random.nextFloat(),
                                           random.nextFloat(),
                                           random.nextFloat()));
      }
    });
    
    // Button to change font color
    JButton fontColor = new JButton("Font white/black");
    fontColor.addActionListener(new ActionListener(){
      public void actionPerformed(ActionEvent e){
        if(text.getForeground() == Color.BLACK)
          text.setForeground(Color.WHITE);
        else
          text.setForeground(Color.BLACK);
      }
    });
    
    // Text buttons
    JPanel north = new JPanel();
    north.setLayout(new BorderLayout());
    north.add(choiceButton, BorderLayout.NORTH);
    north.add(clearText, BorderLayout.SOUTH);
    
    // Color buttons
    JPanel south = new JPanel();
    south.setLayout(new BorderLayout());
    south.add(colorButton, BorderLayout.NORTH);
    south.add(fontColor, BorderLayout.SOUTH);
    
    // Main panel
    panel = new JPanel();
    panel.setPreferredSize(new Dimension(600, 600));
    panel.setLayout(new BorderLayout());
    panel.add(north, BorderLayout.NORTH);
    panel.add(text, BorderLayout.CENTER);
    panel.add(south, BorderLayout.SOUTH);
    
    // Frame
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    Container surface = getContentPane();
    surface.add(panel);
    pack();
    setLocationRelativeTo(null);
    setAlwaysOnTop(true);
    setVisible(true);
    setAlwaysOnTop(false);
  }
  
  private void reader(File file){
    try{
      Scanner scan = new Scanner(file);

      while(scan.hasNextLine()){
        line = scan.nextLine().replaceAll("[^a-zA-Z ]", "").toLowerCase().split("\\s");
        for(int i = 0; i < line.length; i++){
          if(words.containsKey(line[i]))
            words.put(line[i], words.get(line[i]) + 1);
          else if(!line[i].isEmpty())
            words.put(line[i], 1);
          else;
          // Move Along!
        }
      }
    }catch(FileNotFoundException ex){ text.setText("Unable to open file.");}
    if(words.isEmpty())
      text.setText("File is empty.");
    else
      text.setText(words.toString());
  }
  
  public static void main(String[] args){
    new WordCount();
  }
}