//Plate.java

public class Plate {
	int weight;
	int numOf;
	
	public static void main(String[] args){
		new Plate();
	}// end main
	
	public Plate(){
		weight = 900;
		numOf = 0;
	}// end constructor
	
	public Plate(int weight){
		this.weight = weight;
	}// end overloaded constructor
	
	public int getWeight(){
		return weight;
	}// end getWeight
	
	public int getNumOf(){
		return numOf;
	}// end get numOf
	
	public void addOne(){
		numOf++;
	}
}// end Plate
