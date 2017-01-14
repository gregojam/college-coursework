//The Interface!!!

import java.awt.*;
import javax.swing.*;
import java.awt.event.*;

public class Barbell extends JFrame implements ActionListener{

 JLabel barWeightLbl = new JLabel("Bar Weight: ", JLabel.CENTER);
 JLabel targetLbl = new JLabel("Weight Wanted: ", JLabel.CENTER);
 JLabel platesLbl = new JLabel("Plates per side");
 JLabel availableLbl = new JLabel("Available?");
 JLabel totalLbl = new JLabel("Total Weight:", JLabel.RIGHT);
 JLabel fortyFive = new JLabel("0", JLabel.CENTER);
 JLabel thirtyFive = new JLabel("0", JLabel.CENTER);
 JLabel twentyFive = new JLabel("0", JLabel.CENTER);
 JLabel ten = new JLabel("0", JLabel.CENTER);
 JLabel five = new JLabel("0", JLabel.CENTER);
 JLabel twoPointFive = new JLabel("0", JLabel.CENTER);
 JLabel total = new JLabel("0", JLabel.CENTER);
 JTextField target = new JTextField("0");
 JTextField barInput = new JTextField("45");
 static JCheckBox fortyFiveCheck = new JCheckBox("45's:", true);
 static JCheckBox thirtyFiveCheck = new JCheckBox("35's:", true);
 static JCheckBox twentyFiveCheck = new JCheckBox("25.s:", true);
 static JCheckBox tenCheck = new JCheckBox("10's", true);
 static JCheckBox fiveCheck = new JCheckBox("5's:", true);
 static JCheckBox twoPointFiveCheck = new JCheckBox("2.5's:", true);
 JButton commence = new JButton("Load");
 
 public static void main(String[] args){
  new Barbell();
 }// end main
 
 public Barbell(){
  super("Barbell Loader");
  this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
  
  Container surface = this.getContentPane();
  surface.setLayout(new GridLayout(0,4));
  
  surface.add(new JLabel());
  surface.add(new JLabel());
  
  surface.add(availableLbl);
  surface.add(platesLbl);
  
  surface.add(new JLabel());
  surface.add(new JLabel());
  
  surface.add(fortyFiveCheck);
  surface.add(fortyFive);
  
  surface.add(barWeightLbl);
  surface.add(barInput);
  
  surface.add(thirtyFiveCheck);
  surface.add(thirtyFive);
  
  surface.add(targetLbl);
  surface.add(target);
  
  surface.add(twentyFiveCheck);
  surface.add(twentyFive);
  
  surface.add(new JPanel());
  surface.add(commence);
  
  surface.add(tenCheck);
  surface.add(ten);
  
  surface.add(new JPanel());
  surface.add(new JPanel());
  
  surface.add(fiveCheck);
  surface.add(five);
  
  surface.add(new JPanel());
  surface.add(new JPanel());
  
  surface.add(twoPointFiveCheck);
  surface.add(twoPointFive);
  
  surface.add(new JLabel());
  surface.add(new JLabel());
  
  surface.add(totalLbl);
  surface.add(total);
  
  commence.addActionListener(this);
  fortyFiveCheck.addActionListener(this);
  thirtyFiveCheck.addActionListener(this);
  twentyFiveCheck.addActionListener(this);
  tenCheck.addActionListener(this);
  fiveCheck.addActionListener(this);
  twoPointFiveCheck.addActionListener(this);
  this.pack();
  this.setLocationRelativeTo(null);
  this.setVisible(true);
  
 }//end constructor
 
 public void actionPerformed(ActionEvent e){
  Bar bar = new Bar();
  
  int barIn = Integer.valueOf(barInput.getText());
  bar.setBarWeight(barIn);

  int goalWeight = Integer.valueOf(target.getText());
  bar.addPlates(goalWeight);
  
  fortyFive.setText(String.valueOf(bar.getFortyFives()));
  thirtyFive.setText(String.valueOf(bar.getThirtyFives()));
  twentyFive.setText(String.valueOf(bar.getTwentyFives()));
  ten.setText(String.valueOf(bar.getTens()));
  five.setText(String.valueOf(bar.getFives()));
  twoPointFive.setText(String.valueOf(bar.getTwoPointFives()));
  total.setText(String.valueOf(bar.getTotalWeight()));
 }
}//end Barbell
