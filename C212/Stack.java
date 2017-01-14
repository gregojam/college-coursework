//////////////////////////////
//
//C212 Spring 2016
//HW3
//
//Author: James Gregory
//Last Modified: 2/12/16
//
//////////////////////////////

import java.util.NoSuchElementException;
import java.lang.UnsupportedOperationException;
import java.util.Iterator;

public class Stack<Item> implements Iterable{
  private Item[] stack;
  private int size; // number of items in stack
  private int N; // index of last item in stack
  
  // Constructor
  public Stack(){
    stack = (Item[])new Object[2];
    size = 0;
    N = 0;
  }
  
  private class StackIterator implements Iterator<Item>{
    private int ndex;
    
    public StackIterator(){
      if(size == 0)
        throw new NoSuchElementException();
      else
        ndex = N;
    }
    
    public Item next(){
      if(ndex < 0)
        throw new NoSuchElementException();
      else
        return stack[ndex--];
    }
    
    public boolean hasNext(){
      return (ndex >= 0);
    }
    
    public void remove(){
      throw new UnsupportedOperationException();
    }
  }
        
  
  // determines whether Stack is empty
  public boolean isEmpty(){
    for(Object item : stack){
      if(item == null);// Keep Going
      else
        return false;
    }
    return true;
  }
  
  // get size
  public int size(){
    return size;
  }
  
  // inserts an item into Stack
  public void push(Item item){
    resize();
    if(stack[0] == null)
      stack[0] = item;
    else
      stack[++N] = item;
    size++;
  }
  
  // removes an item from Stack
  public Item pop(){
    if(size != 0){
      Item item = stack[N];
      stack[N] = null;
      size--;
      N--;
      resize();
      return item;
    }
    else
      throw new NoSuchElementException();
  }
  
  // appropriately sizes Stack
  private void resize(){
    if(size == stack.length){
      Item[] temp = stack;
      stack = (Item[])new Object[size * 2];
      size = 0;
      N = 0;
      for(int i = 0; i < temp.length; i++){
        stack[i] = temp[i];
        size++;
        N = size - 1;
      }
    }
    else if(stack.length > 2 && size * 4 <= stack.length){
      Item[] temp = (Item[])new Object[size * 2];
      size = 0;
      N = 0;
      for(int i = 0; stack[i] != null; i++){
        temp[i] = stack[i];
        size++;
        N = size -1;
      }
      stack = temp;
    }
    else; // Do Nothing
    if(N < 0)
      N = 0;
    else; // Do Nothing
  }
  
  // Returns the array as a string
  public String toString(){
    String s = "(";
    for(int i = 0; i < stack.length; i++){
      s += stack[i];
      if(i != stack.length - 1)
        s += ", ";
      else; // Do Nothing
    }
    return s + ")";
  }
  
  public StackIterator iterator(){
    return new StackIterator();
  }
  
  public static void main(String[] args){
    Stack s = new Stack();
    Iterator iterator;
    for(int i = 0; i < 5; i++)
      s.push((int) i);
    System.out.println("\nTest Block" + "\n==========" +
                       "\nT1 " + 
                       s.toString().equals("(0, 1, 2, 3, 4, null, null, null)"));
    
    try{
      iterator = s.iterator();
      while(iterator.hasNext()){
        iterator.next();
      }
      System.out.println("T2 true");
      
      iterator.remove();
      System.out.println("T3 false");
    } catch(NoSuchElementException e){
      System.out.println("T2 false");
    } catch(UnsupportedOperationException e){
      System.out.println("T3 true");
    }
    
    for(int i = 0; i < 3; i++)
      s.pop();
    System.out.println("T4 " + s.toString().equals("(0, 1, null, null)"));
    while(s.size() != 0)
      s.pop();
    
    try{
      s.pop();
      System.out.println("T5 false");
    } catch(NoSuchElementException e){
      System.out.println("T5 true");
    }
    
    System.out.println("T6 " + s.toString().equals("(null, null)") +
                       "\nT7 " + (s.size == 0 && s.N == 0));
    
    try{
      iterator = s.iterator();
      System.out.println("T8 false");
    } catch(NoSuchElementException e){
      System.out.println("T8 true");
    }
    
    System.out.println("==========\n");
  }
}