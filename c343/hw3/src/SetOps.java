/**
 * hw3: Problem 2 starter code.
 */

public class SetOps {

	/**
	 * Returns a list (without duplicates) containing all the items
	 * in ls1 plus all the items in ls2. Note: ls1 and ls2 are 
	 * unchanged by this method.
	 * 
	 * Running Time = O(nm), where...
	 * 		n = the sum of the lengths of ls1 and ls2
	 * 		m = length of uni
	 * 
	 * In order to ensure no duplicates are added to uni, we must
	 * check that each element of each input list are not already
	 * in uni before adding them. The contains() method runs in O(m)
	 * time, which dominates the constant time add() method.
	 * 
	 */
	public static <T> List<T> union(List<T> ls1, List<T> ls2) {
		// TODO
		List<T> uni = new DoublyLinkedList();
		for(T e : ls1)
			if(!uni.contains(e))
				uni.add(e);
		for(T e : ls2)
			if(!uni.contains(e))
				uni.add(e);
		return uni;
	}

	/**
	 * Returns a list (without duplicates) of all the items which
	 * appear both in ls1 and in ls2. Note: ls1 and ls2 are
	 * unchanged by this method.
	 * 
	 * Running Time = O(nm) where...
	 * 		n = the length of ls1
	 * 		m = the length of ls2
	 * 
	 * For each element in ls1, this algorithm checks if the element
	 * exists in ls2 using the contains() method, and adds the element
	 * to inter if it does.
	 */
	public static <T> List<T> intersection(List<T> ls1, List<T> ls2) {
		// TODO
		List<T> inter = new DoublyLinkedList();
		for(T e : ls1)
			if(ls2.contains(e))
				inter.add(e);
		return inter;
	}

	/**
	 * Simple testing to get you started. Add more tests of your own!
	 */
	public static void main(String... args) {
		List<String> ls1 = new DoublyLinkedList<>();
		ls1.add("ant");
		ls1.add("bat");
		ls1.add("cat");
		ls1.add("ant");  // this is a duplicate element
		ls1.add("fox");
		int n1 = ls1.size();
		System.out.println("ls1 = " + ls1);

		List<String> ls2 = new DoublyLinkedList<>();
		ls2.add("cat");
		ls2.add("dog");
		ls2.add("dog");  // this is a duplicate element
		ls2.add("emu");
		ls2.add("fox");
		ls2.add("gnu");
		int n2 = ls2.size();
		System.out.println("ls2 = " + ls2);

		List<String> ls3, ls4;
		ls3 = union(ls1, ls2);
		assert n1 == ls1.size();
		assert n2 == ls2.size();
		assert 7 == ls3.size();
		System.out.println("ls3 = " + ls3);

		ls4 = intersection(ls1, ls2);
		assert n1 == ls1.size();
		assert n2 == ls2.size();
		assert 2 == ls4.size();
		System.out.println("ls4 = " + ls4);
		
		List<String> ls5 = union(ls1, ls3);
		for(int i = 0; i < ls5.size(); i ++)
			assert ls3.get(i).equals(ls5.get(i));
		System.out.println("ls5 = " + ls5);
		
		List<Integer> ls6 = new DoublyLinkedList<>();
		ls6.add(2);
		ls6.add(4);
		ls6.add(3);
		ls6.add(14);
		n1 = ls6.size();
		System.out.println("ls6 = " + ls6);
		
		List<Integer> ls7 = new DoublyLinkedList<>();
		ls7.add(8);
		ls7.add(43);
		ls7.add(2);
		ls7.add(3);
		ls7.add(9999);
		n2 = ls7.size();
		System.out.println("ls7 = " + ls7);
		
		List<Integer> ls8, ls9;
		ls8 = union(ls6, ls7);
		assert n1 == ls6.size();
		assert n2 == ls7.size();
		assert 7 == ls8.size();
		System.out.println("ls8 = " + ls8);

		ls9 = intersection(ls6, ls7);
		assert n1 == ls6.size();
		assert n2 == ls7.size();
		assert 2 == ls9.size();
		System.out.println("ls9 = " + ls9);
	}
}

