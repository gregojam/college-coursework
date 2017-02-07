/**
 * We implement a slightly modified version of the algorithm described
 * in Problem 2.26 (pg 54) of Weiss. This modification handles
 * odd-length arrays.
 * 
 * A majority element in an array, A, of size n is an element that
 * appears n / 2 times (thus, there is at most one).
 * 
 * Algorithm: First, identify one or two candidates for the majority
 * element using the process described below. If no candidates are
 * identified during this step, then there is no majority in A, so
 * stop. Otherwise, for each candidate, determine if it is actually
 * the majority using a sequential search through the array.
 * 
 * To find the candidates in the array A, form a second array B. Then
 * compare A_1 and A_2. If they are equal, add one of these to B;
 * otherwise do nothing.  Then compare A_3 and A_4. Again if they are
 * equal, add one of these to B; otherwise do nothing. Continue in
 * this fashion until the entire array is read.  If the length of A is
 * odd, then add the last element to B. Then recursively find the one
 * or two majority candidates for B; these will be candidates for A.
 */

public class Majority {
  
  /**
   * A Node contains an int, a pointer to the previous node in the list, 
   * and a pointer to the next Node in the list.
   */
  static class Node{
    int data;
    Node parent;
    Node next;
    
    public Node(int d, Node p){
      data = d;
      parent = p;
      next = null;
    }
  }
  
  /**
   * An DoublyLinkedList is a dynamic list of Nodes.
   */
  static class DoublyLinkedList{
    Node head;
    int n;
    
    public DoublyLinkedList(){
      head = null;
    }
    
    /**
     * Adds value x to the list
     */
    public void add(int x){
      n++;
      if(head == null)
        head = new Node(x, null);
      else{
        Node p = head;
        while(p.next != null)
          p = p.next;
        p.next = new Node(x, p);
      }
    }
    
    /**
     * Removes Node a from the list.
     */
    public void remove(Node a){
    	n--;
    	if(a == null)
    		throw new IllegalRemoveException();
    	if(a.parent == null){
    		a.next.parent = null;
    		head = a.next;
    	}
    	else{
    		a.next.parent = a.parent;
    		a.parent.next = a.next;
    	}
    }
    
    /**
     * Removes concurrent Nodes a and b from the list.
     */
    public void remove2(Node a, Node b){
    	n -= 2;
    	if(a == null || b == null || a.next != b)
    		throw new IllegalRemoveException();
    	if(a.parent == null){
    		if(b.next == null)
    			head = null;
    		else{
    			b.next.parent = null;
    			head = b.next;
    		}
    	}
    	else{
    		a.parent.next = b.next;
    		if(b.next != null)
    			b.next.parent = a.parent;
    	}
    }
    
    /**
     * Returns the list as an array.
     */
    public int[] toArray(){
      if(head == null)
        return null;
      int[] arr = new int[n];
      Node p = head;
      for(int i = 0; i < n; i++){
        arr[i] = p.data;
        p = p.next;
      }
      return arr;
    }
    
    /**
     * Returns the list in a pretty string.
     */
    public String toString(){
    	Node p = head;
    	String str = "{";
    	for(int i = 0; i < n; i++){
    		str += p.data;
    		if(i != n-1)
    			str += " ";
    		p = p.next;
    	}
    	return str + "}";
    }
  }

  /**
   * Returns the majority element in a, if one exists. Otherwise, a
   * NoMajorityException is thrown.
   */
  public static int majority(int[] a) {
    int[] b = findCandidates(a);
    assert b.length <= 2;
    for (int x : b)
      if (isMajority(x, a))
        return x;
    throw new NoMajorityException();
  }
  
  /**
   * Returns an array of 0, 1, or 2 candidates for majority element in
   * a. Implement the recursive algorithm described in the main
   * comment.
   */
  public static int[] findCandidates(int[] a) {
	if(a.length <= 2)
		return a;
	
    DoublyLinkedList b = new DoublyLinkedList();
    for(int i = 0; i < a.length; i += 2){
      if(i == a.length-1)
        b.add(a[i]);
      else if(a[i] == a[i+1])
        b.add(a[i]);
    }
    listFind(b);
    return b.toArray();
  }
  
  /**
   * Compares each odd indexed element of a DoubleLinkedList to the following element.
   * If they are equal, removes one.
   * If they are not equal, removes both.
   * If there is not a following element to compare to, the element is kept.
   */
  public static void listFind(DoublyLinkedList a){
	  while(a.n > 2){
		  Node p = a.head;
		  Node k = null;
		  int m = a.n;
		  for(int i = 0; i < m-1; i += 2){
			  k = p;
			  p = p.next.next;
			  if(k.data != k.next.data){
				  a.remove2(k, k.next);
			  }
			  else if(k.data == k.next.data)
				  a.remove(k);
		  }
	  }
  }
  
  /**
   * Returns true iff x appears in more than half of the elements of a.
   */
  public static boolean isMajority(int x, int[] a) {
    int count = 0, n = a.length;
    for (int i = 0; i < n; i++)
      if (x == a[i])
        count++;
    return count > n / 2;
  }

  /**
   * Asserts that the majority exists in a and is the same as expected.
   */
  public static void checkExpect(int expected, int[] a) {
    try {
      assert expected == majority(a);
    }
    catch (NoMajorityException ex) {
      assert false;
    }
  }
  
  /**
   * Asserts that a has no majority element.
   */
  public static void checkException(int[] a) {
    try {
      majority(a);
      assert false;
    }
    catch (NoMajorityException ex) {
      assert true;
    }
  }
  
  /**
   * Run some tests.
   */
  public static void main(String... args) {
    checkExpect(4, new int[] { 3, 3, 4, 2, 4, 4, 2, 4, 4 });
    checkException(new int[] { 3, 3, 4, 2, 4, 4, 2, 4 });
    checkExpect(1, new int[] { 1, 2, 1, 2, 1, 2, 1, 2, 1 });
    checkExpect(2, new int[] { 1, 2, 1, 2, 1, 2, 1, 2, 2 });
    checkException(new int[] { 1, 2, 1, 2, 1, 2, 1, 2, 3 });
    checkExpect(1, new int[] { 
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        });
    checkException(new int[] { });
    checkExpect(5, new int[] { 5 });
    checkException(new int[] { 1, 2 });
    checkExpect(1, new int[] { 1, 1 });
    checkException(new int[] { 1, 2, 3 });
    checkExpect(1, new int[] { 1, 1, 2 });
    checkExpect(1, new int[] { 1, 2, 1 });
    checkExpect(1, new int[] { 2, 1, 1 });
    checkExpect(1, new int[] { 1, 1, 1 });
    
    // Testing DoublyLinkedList
    DoublyLinkedList a = new DoublyLinkedList();
    a.add(1);
    a.add(2);
    a.add(3);
    a.add(4);
    assert "{1 2 3 4}".equals(a.toString());
    Node p = a.head;
    Node k = p.next;
    a.remove2(p, k);
    assert "{3 4}".equals(a.toString());
    a.add(5);
    a.add(6);
    assert "{3 4 5 6}".equals(a.toString());
    p = a.head.next;
    k = p.next;
    a.remove2(p,k);
    assert "{3 6}".equals(a.toString());
    a.add(8);
    a.add(7);
    a.add(7);
    assert "{3 6 8 7 7}".equals(a.toString());
    p = a.head.next.next;
    k = p.next;
    a.remove2(p,k);
    assert "{3 6 7}".equals(a.toString());
    listFind(a);
    assert "{7}".equals(a.toString());    
    a.add(6);
    a.add(6);
    a.add(6);
    a.add(6);
    a.add(7);
    listFind(a);
    assert "{6}".equals(a.toString());
  }
}

class NoMajorityException extends RuntimeException {}
class IllegalRemoveException extends RuntimeException{}
