
public class DequeList<T> {
	DoublyLinkedList<T> list;
	
	/**
	 * Converts array arr to a new DequeList.
	 * @param arr
	 */
	DequeList(T[] arr){
		list = new DoublyLinkedList<>();
		for(T e : arr){
			inject(e);
		}
	}
	
	/**
	 * Constructs an empty DequeList.
	 */
	DequeList(){
		list = new DoublyLinkedList<>();
	}
	
	/**
	 * Adds the value of x to the front of this list.
	 * @param x
	 */
	public void push(T x){
		list.addAt(x, 0);
	}
	
	/**
	 * Removes the value at the front of this list, and
	 * returns it.
	 */
	public T pop(){
		return list.remove(0);
	}
	
	/**
	 * Adds the value of x to the end of this list.
	 * @param x
	 */
	public void inject(T x){
		list.add(x);
	}
	
	/**
	 * Removes the value at the end of this list, and
	 * returns it.
	 */
	public T eject(){
		return list.removeLast();
	}
	
	public String toString(){
		return list.toString();
	}
	
	public static void main(String[] args){
		DequeList<Integer> lst1 = new DequeList<>();
		lst1.push(1);
		lst1.push(4);
		lst1.push(8);
		assert "(8 4 1)".equals(lst1.toString());
		lst1.pop();
		assert "(4 1)".equals(lst1.toString());
		lst1.inject(7);
		assert "(4 1 7)".equals(lst1.toString());
		lst1.eject();
		assert "(4 1)".equals(lst1.toString());
		lst1.push(lst1.eject());
		assert "(1 4)".equals(lst1.toString());
		
		DequeList<String> lst2 = new DequeList<>();
		lst2.push("test");
		lst2.push("a");
		lst2.push("is");
		lst2.push("this");
		assert "(this is a test)".equals(lst2.toString());
		lst2.pop();
		assert "(is a test)".equals(lst2.toString());
		lst2.inject("string");
		assert "(is a test string)".equals(lst2.toString());
		lst2.eject();
		assert "(is a test)".equals(lst2.toString());
		lst2.push(lst2.eject());
		assert "(test is a)".equals(lst2.toString());
	}
}

/**
 * A Deque has to do this stuff.
 */
interface Deque<T> {
	void push(T x);
	T pop();
	void inject(T x);
	T deject();
}