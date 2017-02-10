/**
 * hw3: Problem 1 starter code.
 * 
 * Hallmarks of a DoublyLinkedList:
 * - headnode (also called a dummy node)
 * - backward pointers to the previous node in the list
 * - circularity: last node points forward to the headnode and the
 *                headnode points backward to the last node
 */

import java.util.Iterator;

public class DoublyLinkedList<T> implements List<T> {

	/**
	 * Node is a pair containing a data field and pointers to
	 * the previous and next nodes in the list.
	 */
	class Node {
		T data;
		Node next, prev;

		Node(T data) {
			this(data, null, null);
		}

		Node(T data, Node prev, Node next) {
			this.data = data;
			this.prev = prev;
			this.next = next;
		}
	}

	Node head;  // always points to the headnode for this list
	int n;      // the number of nodes in this list, initially 0

	/**
	 * Creates the empty list. 
	 */
	public DoublyLinkedList() {
		// TODO: Create the headnode.
		// Note that the prev and next fields in the headnode should 
		// point back to the headnode.
		head = new Node(null);
		head.next = head;
		head.prev = head;
		n = 0;
	}

	/**
	 * Inserts the value x at the end of this list.
	 * @throw AddNullException iff x is null
	 */
	public void add(T x) {  
		// TODO: This must run in O(1) time.
		if(x == null)
			throw new AddNullException();
		n++;
		Node oldLast = head.prev;
		Node newLast = new Node(x, oldLast, head);
		head.prev = newLast;
		oldLast.next = newLast;
	}
	
	/**
	 * Inserts the values in x at the end of this list.
	 * @throw AddNullException iff x contains null, but adds 
	 * all prior elements of x.
	 */
	public void add(T... x){
		for(T e : x)
			add(e);
	}
	
	/**
	 * Inserts the value x at index i in the list.
	 * @throw IndexOutOfBoundsException iff i in out of range for this list.
	 * @throw AddNullException iff x is null.
	 */
	public void addAt(T x, int i){
		if (i < 0 || i >= size() + 1)
			throw new IndexOutOfBoundsException();
		if(x == null)
			throw new AddNullException();
		n++;
		Node curr = head.next;
		for(int j = 0; j < i; j++)
			curr = curr.next;
		Node added = new Node(x, curr.prev, curr);
		curr.prev.next = added;
		curr.prev = added;
	}

	/**
	 * Removes the element at index i from this list.
	 * @return the data in the removed node.
	 * @throw IndexOutOfBoundsException iff i is out of range for this list.
	 */
	public T remove(int i) {
		if (i < 0 || i >= size())
			throw new IndexOutOfBoundsException();
		// TODO: Don't forget to skip over the headnode.
		n--;
		Node removed = head.next;
		for(int j = 0; j < i; j++)
			removed = removed.next;
		removed.prev.next = removed.next;
		removed.next.prev = removed.prev;
		return removed.data;
	}
	
	/**
	 * Removes the last value of this list, and returns it.
	 * @throw EmptyListException iff list is empty prior to call.
	 */
	public T removeLast(){
		if(n == 0)
			throw new EmptyListException();
		n--;
		Node removed = head.prev;
		removed.prev.next = head;
		head.prev = removed.prev;
		return removed.data;
	}

	/**
	 * Returns the i-th element from this list, where i is a zero-based index.
	 * @throw IndexOutOfBoundsException iff i is out of range for this list.
	 */
	public T get(int i) {
		if (i < 0 || i >= size())
			throw new IndexOutOfBoundsException();
		// TODO: Don't forget to skip over the headnode.
		Node curr = head.next;
		for(int j = 0; j < i; j++)
			curr = curr.next;
		return curr.data;
	}

	/**
	 * Returns true iff the value x appears somewhere in this list.
	 */
	public boolean contains(T x) {
		// TODO: Don't forget to skip over the headnode.
		Node curr = head.next;
		for(int i = 0; i < n; i++){
			if(curr.data.equals(x))
				return true;
			curr = curr.next;
		}
		return false;
	}

	/**
	 * Returns the number of elements in this list.
	 */
	public int size() {
		return n;
	}

	/**
	 * Returns an iterator for this list.
	 */
	public Iterator<T> iterator() {
		return new Iterator<T>() {
			Node curr = head;
			
			public boolean hasNext() {
				// TODO	
				return curr.next.data != null;
			}

			public T next() {
				// TODO
				curr = curr.next;
				return curr.data;
			}

			public void remove() {
				// TODO: This must run in O(1) time.
				n--;
				curr.prev.next = curr.next;
				curr.next.prev = curr.prev;
			}
		};
	}

	/**
	 * Returns a string representing this list (resembling a Racket list).
	 */
	public String toString() {
		if (isEmpty())
			return "()";    
		Iterator<T> it = iterator();
		StringBuilder ans = new StringBuilder("(").append(it.next());
		while (it.hasNext())
			ans.append(" ").append(it.next());
		return ans.append(")").toString();
	}

	/**
	 * Simple testing to get you started. Add more tests of your own!
	 */
	public static void main(String... args) {
		DoublyLinkedList<Integer> xs = new DoublyLinkedList<>();
		int[] a = new int[] { 4, 3, 6, 5, 7, 8 };
		for (int x : a)
			xs.add(x);
		assert 6 == xs.size();
		for (int i = 0; i < a.length; i++)
			assert xs.get(i) == a[i];
		assert !xs.contains(null);
		for (int x : a)
			assert xs.contains(x);
		assert "(4 3 6 5 7 8)".equals(xs.toString());
		assert xs.remove(0) == 4;
		assert xs.remove(1) == 6;
		assert 4 == xs.size();
		assert "(3 5 7 8)".equals(xs.toString());
		while (!xs.isEmpty())
			xs.remove(xs.size() - 1);
		assert 0 == xs.size();
		for (int x : a)
			xs.add(x);
		for (int x : xs)
			assert xs.contains(x);
		Iterator<Integer> it = xs.iterator();
		while (it.hasNext())
			if (it.next() % 2 == 0)
				it.remove();
		assert 3 == xs.size();
		assert "(3 5 7)".equals(xs.toString());
		System.out.println("Integer tests passed...");
		
		DoublyLinkedList<String> strs = new DoublyLinkedList<>();
		String[] b = new String[] { "Hello,",  "how", "are", "you?" };
		strs.add(b);
		assert 4 == strs.size();
		for (int i = 0; i < b.length; i++)
			assert strs.get(i).equals(b[i]);
		assert !xs.contains(null);
		for (String s : b)
			assert strs.contains(s);
		assert "(Hello, how are you?)".equals(strs.toString());
		assert strs.remove(0).equals("Hello,");
		assert strs.remove(1).equals("are");
		assert 2 == strs.size();
		assert "(how you?)".equals(strs.toString());
		while (!strs.isEmpty())
			strs.remove(strs.size() - 1);
		assert 0 == strs.size();
		for (String s : b)
			strs.add(s);
		for (String s : strs)
			assert strs.contains(s);
		Iterator<String> it2 = strs.iterator();
		while (it2.hasNext())
			if (it2.next().length() == 3)
				it2.remove();
		assert 2 == strs.size();
		assert "(Hello, you?)".equals(strs.toString());
		strs.addAt("who", 1);
		strs.addAt("are", 2);
		assert 4 == strs.size();
		assert "(Hello, who are you?)".equals(strs.toString());
		strs.removeLast();
		strs.removeLast();
		assert 2 == strs.size();
		assert "(Hello, who)".equals(strs.toString());
		System.out.println("String tests passed...");
		
		System.out.println("All tests passed...");
	}
}

/**
 * If you want to call yourself a List, then implement this interface:
 */
interface List<T> extends Iterable<T> {
	void add(T x);  // simple add
	T remove(int i);
	T get(int i);
	boolean contains(T x);
	int size();
	default boolean isEmpty() {
		return size() == 0;
	}
}

class AddNullException extends NullPointerException{}
class EmptyListException extends RuntimeException{}
