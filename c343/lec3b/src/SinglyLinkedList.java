/**
 * Here is the List interface and SinglyLinkedList class from
 * Lecture 3a. Do the following:
 * 
 * (1) Add appropriate Javadoc style comments to all methods.
 * (2) Implement the remove() method.
 * (3) Implement the toString() method.
 * (4) Turn List into a generic interface and SinglyLinkedList into
 *     a generic class so that the structure can be used to hold
 *     any type T of data.
 * (5) Test thoroughly in main() with a variety of data types.
 *
 * We affirm that all members of our team contributed to this solution.
 * @author Red7
 * @author Derek Erwin
 */

public class SinglyLinkedList<T> implements List<T> {

  class Node{
    T data;
    Node next;
    
    Node(T data) {
      this(data, null);
    }
    
    Node(T data, Node next) {
      this.data = data;
      this.next = next;
    }
  }
  
  Node head;
  int n;
  
  /**
   * Adds a new element, x, to the List
   */
  public void add(T x) {
    n++;
    if (head == null) 
      head = new Node(x);
    else {
      Node p = head;
      while (p.next != null)
        p = p.next;
      p.next = new Node(x);
    }
  }
  
  /**
   * Removes the ith element from the List.
   * Returns the data of the removed element.
   */
  public T remove(int i) {
    if(i < 0 || i >= size())
        throw new IndexOutOfBoundsException();
    n--;
    Node p = head;
    Node parent = null;
    while(i > 0){
        parent = p;
        p = p.next;
        i--;
    }
    if(parent != null)
    	parent.next = p.next;
    else
    	head = p.next;
    return p.data;
  }
  
  /**
   * Returns the ith element's data from the List.
   */
  public T get(int i) {
    if (i < 0 || i >= size())
      throw new IndexOutOfBoundsException();
    Node p = head;
    while (i > 0) {
      p = p.next;
      i--;
    }
    return p.data;
  }

  /**
   * Returns whether or not element x exists within the List
   */
  public boolean contains(T x) {
    Node p = head;
    while (p != null){
      if (p.data.equals(x))
        return true;
      else
        p = p.next;
    }
    return false;
  }
  
  /**
   * Returns the number of elements in the List.
   */
  public int size() {
    return n;
  }
  
  /**
   * Returns whether or not the List is empty.
   */
  public boolean isEmpty() {
    return size() == 0;
  }
  
  /**
   * Converts the List to an easy-to-read String.
   */
  public String toString() {
    String data = "(";
    Node p = head;
    while (p != null) {
        data += p.data + " ";
        p = p.next;
    }
    int dataLength = data.length();
    if(dataLength > 1) // If we added any data, remove extra whitespace character
        data = data.substring(0, dataLength - 1);
    data += ")";
    return data;
  }
  
  /**
   * Test bracket
   * @param args
   */
  public static void main(String... args) {
    List xs = new SinglyLinkedList();
    assert "()".equals(xs.toString());
    int[] a = new int[] { 7, 4, 6, 9, 2 };
    for (int x : a)
      xs.add(x);
    assert "(7 4 6 9 2)".equals(xs.toString());
    for (int x : a)
      assert xs.contains(x);
    for (int i = 0; i < xs.size(); i++)
      assert xs.get(i).equals(a[i]);
    assert "(7 4 6 9 2)".equals(xs.toString());
    xs.remove(3);
    assert "(7 4 6 2)".equals(xs.toString());
    int i = 0;
    a = new int[] {7, 4, 6, 2};
    while (!xs.isEmpty())
      assert xs.remove(0).equals(a[i++]);
    assert "()".equals(xs.toString());
  }
}

interface List<T> {
  void add(T x);
  T remove(int i);
  T get(int i);
  boolean contains(T x);
  int size();
  boolean isEmpty();
}
