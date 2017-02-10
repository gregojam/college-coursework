/**
 * Tree interface and BinarySearchTree class from lec4b.
 *                          ^^^^^^
 * 
 * TODO: Pre-Lecture Exercise for lec5a.
 * Note: This is a team effort. Every member of your team is expected to 
 *       make non-trivial contributions towards your solution.
 * 
 * Make the following modifications:
 * (1) Add javadoc style comments to all methods.
 * (2) Implement BinarySearchTree.contains() so that it runs in O(h) time,
 *     where h is the height of the tree.
 * (3) Modify Tree and BinarySearchTree so that they are generic for any 
 *     Comparable type object.
 * (4) [challenge] Try to implement a sensible BinarySearchTree.toString()
 *      method. Recall that an inorder traversal of a BST yields a sorted 
 *      sequence.
 * (5) Test thoroughly in main(). Be sure to include tests on non-integer 
 *     data.
 * 
 * @author <red7>
 * @author <James Gregory>
 */

public class BinarySearchTree<T extends Comparable<T>> implements Tree<T> {
  
  class Node {
    T data;
    Node left, right;
    
    // Constructs  node using the information given as the key with two null children 
    Node(T key) {
      this(key, null, null);
    }
    
    // Constructs a Node containing information, as well as a child node on its left branch
	// and a child node on its right branch
    Node(T data, Node left, Node right) {
      this.data = data;
      this.left = left;
      this.right = right;
    }
    
    //checks if current node is a leaf by checking if both left and right are null 
    boolean isLeaf() {
      return left == null && right == null;
    }
  }
  
  Node root;
  int n;
  
  // insert adds a node to the Tree
  public void insert(T key) {
    n++;
    root = insertHelper(key, root);
  }
  
  // insert helper finds the correct location to put the node in the Tree
  private Node insertHelper(T key, Node p) {
    if (p == null)
      return new Node(key);
    if (key.compareTo(p.data) < 0)
      p.left = insertHelper(key, p.left);
    else
      p.right = insertHelper(key, p.right);
    return p;
  }
  
  // contains returns true if the given key is in the Tree
  public boolean contains(T key) {
	  Node n = root;
	  while(true){
		  if(n == null)
			  return false;
		  int cmp = key.compareTo(n.data);
		  if(cmp == 0)
			  return true;
		  if(cmp < 0)
			  n = n.left;
		  else
			  n = n.right;
	  }
  }
  
  // Returns the number of nodes in the Tree  
  public int size() {
    return n;
  }
  
  // Helper method to represent bst as a string
  private String toString(Node n){
	  if(n == null)
		  return "";
	  else
		  return (toString(n.left) +
				  n.data.toString() + " " +
				  toString(n.right));
  }
  // returns a clean string representation of the content of the Tree
  public String toString(){
	  return "{ " + toString(root) + "}";
  }
  
  public static void main(String... args) { 
    int[] a = new int[] { 3, 9, 7, 2, 1, 5, 6, 4, 8 };
    Tree<Integer> bst = new BinarySearchTree<>();
    assert bst.isEmpty();
    for (int key : a)
      bst.insert(key);
    /**       3
     *      /   \
     *     2     9
     *    /     /
     *   1     7
     *       /   \
     *      5     8
     *     / \
     *    4   6
     */
    assert !bst.isEmpty();
    assert bst.size() == a.length;
    for (Integer key : a)
    	assert bst.contains(key);
    System.out.println(bst);
    String[] str = new String[]{"the","fox","and","hound"};
    Tree<String> bst2 = new BinarySearchTree<>();
    assert bst2.isEmpty();
    for (String key : str)
    	bst2.insert(key);
    assert !bst2.isEmpty();
    assert bst2.size() == str.length;
    for (String key : str)
    	assert bst2.contains(key);
    assert !bst2.contains("an");
    System.out.println(bst2);
    
    
    
  }
}

interface Tree<T extends Comparable<T>> {
  void insert(T key);
  default void remove(T key) {
    throw new UnsupportedOperationException();
  }
  boolean contains(T key);
  int size();
  default boolean isEmpty() {
    return size() == 0;
  }
}
