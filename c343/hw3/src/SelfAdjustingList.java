/**
 * hw3: Problem 5 starter code.
 */

public class SelfAdjustingList<T> extends DoublyLinkedList<T> {
	
	/**
	 * Adds the value of x to the front of the list.
	 */
	@Override
	public void add(T x){
		super.addAt(x, 0);
	}

	/**
	 * Returns the value at the i-th position in the list, and
	 * moves it to the front of the list.
	 * @param i
	 */
	public T find(int i){
		Node mover = head.next;
		for(int j = 0; j < i; j++)
			mover = mover.next;
		mover.prev.next = mover.next;
		mover.next.prev = mover.prev;
		head.next.prev = mover;
		mover.next = head.next;
		mover.prev = head;
		head.next = mover;
		
		return mover.data;
	}

	/**
	 * Simple testing to get you started. Add more tests of your own!
	 */
	public static void main(String... args) {
		SelfAdjustingList<Integer> xs = new SelfAdjustingList<>();
		for (int x = 1; x <= 10; x++)
			xs.add(x);
		for (int i = 0; i < xs.size(); i++)
			assert 10 - i == xs.get(i);
		for (int i = 0; i < xs.size(); i++) {
			int x = xs.get(i);
			assert x == xs.find(i);
		}
		for (int i = 0; i < xs.size(); i++) {
			int x = xs.find(i);
			assert x == xs.get(0);
		}
		System.out.println("Integer tests passed");
		
		SelfAdjustingList<String> strs = new SelfAdjustingList<>();
		String[] a = new String[]{"burger", "pizza", "fries", "nachos", "pie"};
		for(String s : a){
			strs.add(s);
			assert s.equals(strs.get(0));
		}
		for(int i = 1; i < strs.size(); i += 2){
			String s = strs.find(i);
			assert s == strs.get(0);
		}
		System.out.println("String tests passed");
		
		
		System.out.println("All tests passed...");
	}
}
