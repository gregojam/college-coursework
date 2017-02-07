package lec4a;

/**
* We affirm that all members of our team contributed to this solution.
* @author Red7
* @author James Gregory
*/


/**
 * A LinkedQueue is a first-in-first-out data structure that utilizes a SinglyLinkedList,
 * but restricts some of its functionalities.
 */
public class LinkedQueue <E> implements Queue <E>{
	
	/**
	 * A Node is a pair of an object and the next node in the list
	 *
	 */
	class Node{
	    E data;
	    Node next;
	    
	    Node(E data) {
	      this(data, null);
	    }
	    
	    Node(E data, Node next) {
	      this.data = data;
	      this.next = next;
	    }
	  }
  
	List<E> xs = new SinglyLinkedList<>();
	
	/**
	 * Constructor doesn't initialize anything
	 */
	public LinkedQueue(){};
	
	/**
	 * Adds value x to the list
	 */
	public void enqueue(E x){
		 xs.add(x);
	}
	
	/**
	 * Returns the List's front Node's data and removes the Node from the list.
	 */
	public E dequeue(){
		if(isEmpty())
			throw new EmptyQueueException();
		return xs.remove(0);
	}
	
	/**
	 * Returns the List's front Node's data.
	 */
	public E front(){
		if(isEmpty())
			throw new EmptyQueueException();
		return xs.get(0);
	}
	
	/**
	 * Returns the List's last Node's data.
	 */
	public E back(){
		if(isEmpty())
			throw new EmptyQueueException();
		return xs.get(xs.size()-1);
	}
	
	/**
	 * Returns the number of elements in the list.
	 */
	public int size(){
		return xs.size();
	}
	
	/**
	 * Returns whether or not he list is empty.
	 */
	public boolean isEmpty(){
		return xs.size() == 0;
	}
	
	/**
	 * Returns a pretty string equivalent of this list.
	 */
	public String toString(){
		if(isEmpty())
			throw new EmptyQueueException();
		String str = "{";
		for(int i = 0; i < xs.size(); i++){
			str += xs.get(i);
			if(i != xs.size()-1)
				str += " ";
		}
		str += "}";
		return str;
	}
	
	/**
	 * Testing bracket
	 */
	public static void main(String[] args){
		LinkedQueue<Integer> a = new LinkedQueue<>();
		//Test isEmpty
		assert a.isEmpty();
		
		//Test enqueue and further isEmpty
		a.enqueue(1);
		assert !a.isEmpty();
		a.enqueue(4);
		a.enqueue(8);
		assert "{1 4 8}".equals(a.toString());
		
		//Test dequeue
		a.dequeue();
		assert "{4 8}".equals(a.toString());
		
		//Test front and back
		assert 4 == a.front();
		assert 8 == a.back();
		a.enqueue(7);
		assert 4 == a.front();
		assert 7 == a.back();
		a.enqueue(9);
		assert 4 == a.front();
		assert 9 == a.back();
	}
}

interface Queue <E>{
	  void enqueue(E x);
	  E dequeue();
	  E front();
	  E back();
	  int size();
	  boolean isEmpty();
}

class EmptyQueueException extends RuntimeException{}