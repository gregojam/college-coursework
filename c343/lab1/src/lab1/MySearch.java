package lab1;

public class MySearch {
	
	public static int search(int[] arr, int[] brr){
		// Iterate through array brr, while the number of unexplored elements in brr
		//  is greater than the number of elements in arr
		for (int i = 0; i < brr.length - arr.length+1; i++){
			// Iterate through arr elements until one doesn't match appropriate brr element
			for (int j = 0; j < arr.length; j++){
				if(arr[j] != brr[i+j]) // if elements don't match, restart at next brr element
					break;
				else if(j == arr.length-1) // all elements of a matched, return start location
					return i;
			}
		}
		return -1;
	}

	public static void main(String[] args) {
		int[] a, b;
		int res;
		
		a = new int[] {1, 2, 3};
		b = new int[] {5};
		res = search(a, b);
		if(res != -1)
			System.out.println("Test 1 failed: res = " + res);
		
		a = new int[] {1, 2, 3};
		b = new int[] {5, 6, 3, 2, 1, 2, 3, 7, 8, 0};
		res = search(a, b);
		if(res != 4)
			System.out.println("Test 2 failed: res = " + res);
		
		a = new int[] {1, 2, 3};
		b = new int[] {5, 6, 3, 1, 2, 3, 1, 2, 3};
		res = search(a, b);
		if(res != 3)
			System.out.println("Test 1 failed: res = " + res);
		
		System.out.println("Tests completed");

	}

}
