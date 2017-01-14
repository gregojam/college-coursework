//////////////////////////////
//
//C212 Spring 2016
//Lab 6
//
//Author: James Gregory
//Last Modified: 2/22/16
//
//////////////////////////////

import java.util.*;
import java.io.File;
import java.io.FileNotFoundException;

class FrequencyCounter{
  
  private ArrayList<String> words;
  private ArrayList<Frequency> counter;
  private int numLines;
  private String daFile;
  private String[] line;
  
  // contructor
  public FrequencyCounter(String file){
    daFile = file;
    words = new ArrayList<String>();
    counter = new ArrayList<Frequency>();
    numLines = 0;
    try{
      Scanner scan = new Scanner(new File(file));
      
      // fills 'words' and 'counter' with distinct words and counts lines from text
      // ~~removes punctuation and excludes any non-words~~
      while(scan.hasNextLine()){
        numLines += 1;
        line = scan.nextLine().replaceAll("[^a-zA-Z ]", "").toLowerCase().split("\\s");
        for(int i = 0; i < line.length; i++){
          if(words.contains(line[i]))
            counter.get(words.indexOf(line[i])).add1();
          else if(!line[i].isEmpty()){
            words.add(line[i]);
            counter.add(new Frequency(line[i]));
            }
          else;
          // Move Along!
        }
      }
    }catch(FileNotFoundException e) {
      e.printStackTrace();
    }
  }
  
  // getters
  public String getFile(){
    return daFile;
  }
  
  public int numDisWords(){
    return words.size();
  }
  
  public int numLines(){
    return numLines;
  }
  // end getters
  
  // calculates and returns total number of words in file
  public int numTotWords(){
    int totalWords = 0;
    for(int i = 0; i < counter.size(); i++){
      totalWords += counter.get(i).getCount();
    }
    return totalWords;
  }
  
  // returns 'counter' as a formatted string
  public String disCount(){
    String result = "";
    
    for(int i = 0; i < counter.size(); i++){
      result += counter.get(i).getWord() + ":";
      result += counter.get(i).getCount() + "\n";
    }
    return result + "\n";
  }    
  
  public static void main(String[] args){
    FrequencyCounter a = new FrequencyCounter("tinyTale.txt");
    System.out.println(a.getFile() + " has " + a.numDisWords() + 
                       " distinct words.\n" +
                       a.getFile() + " has " + a.numLines() +
                       " number of lines.\n" +
                       a.getFile() + " has " + a.numTotWords() +
                       " number of words.\n\n" +
                       a.disCount() + "\n");
    
//    FrequencyCounter b = new FrequencyCounter("tale.txt");
//    System.out.println(b.getFile() + " has " + b.numDisWords() +
//                       " distinct words.\n" +
//                       b.getFile() + " has " + b.numLines() +
//                       " number of lines.\n" +
//                       b.getFile() + " has " + b.numTotWords() +
//                       " number of words.\n\n" +
//                       b.disCount() + "\n");
  }
}
  
  