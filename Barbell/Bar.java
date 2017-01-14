//Bar.java

public class Bar {
 int barWeight;
 int totalWeight;
 Plate fortyFives;
 Plate thirtyFives;
 Plate twentyFives;
 Plate tens;
 Plate fives;
 Plate twoPointFives;
 
 public static void main(String[] args){
  new Bar();
 }//end main
 
 public Bar(){
  barWeight = 0;
  totalWeight = 0;
  fortyFives = new Plate();
  thirtyFives = new Plate(700);
  twentyFives = new Plate(500);
  tens = new Plate(200);
  fives = new Plate(100);
  twoPointFives = new Plate(50);
  } // end constructor
 
 public int getBarWeight(){
  return barWeight;
 }//end getWeight
 
 public void setBarWeight(int weight){
  barWeight = weight * 10;
 }// end setWeight
 
 public int getTotalWeight(){
  return totalWeight / 10;
 }// end getTotalWeight
 
 public int getFortyFives(){
  return fortyFives.getNumOf();
 }
 
 public int getThirtyFives(){
  return thirtyFives.getNumOf();
 }
 
 public int getTwentyFives(){
  return twentyFives.getNumOf();
 }
 
 public int getTens(){
  return tens.getNumOf();
 }
 
 public int getFives(){
  return fives.getNumOf();
 }
 
 public int getTwoPointFives(){
  return twoPointFives.getNumOf();
 }
 
 
 public void addPlates(int goalWeight){
  int target;
  int difference;
  
  goalWeight = goalWeight * 10 - barWeight;
  if(Barbell.twoPointFiveCheck.isSelected()){
   target = (goalWeight / 50) * 50;//make target multiple of 5
   difference = goalWeight - target;
  
   if(difference >= 25)
    target += 50;
  }//end 2.5 check
  
  else{
   target = (goalWeight / 100) * 100;//make target multiple of 10
   difference = goalWeight - target;
   
   if(difference >= 50)
    target += 100;
  }//end else

  totalWeight += barWeight;
  
  if(Barbell.fortyFiveCheck.isSelected()){
   while(target >= fortyFives.getWeight()){
    target -= fortyFives.getWeight();
    fortyFives.addOne();
    totalWeight += fortyFives.getWeight();
   }//end add 45s
  }//end if 45s
  
  if(Barbell.thirtyFiveCheck.isSelected()){
   while(target >= thirtyFives.getWeight()){
    target -= thirtyFives.getWeight();
    thirtyFives.addOne();
    totalWeight += thirtyFives.getWeight();
   }// end add 35s
  }//end if 35s
  
  if(Barbell.twentyFiveCheck.isSelected()){
   while(target >= twentyFives.getWeight()){
    target -= twentyFives.getWeight();
    twentyFives.addOne();
    totalWeight += twentyFives.getWeight();
   }// end add 25s
  }//end if 25s
  
  if(Barbell.tenCheck.isSelected()){
   while(target >= tens.getWeight()){
    target -= tens.getWeight();
    tens.addOne();
    totalWeight += tens.getWeight();
   }//end add 10s
  }//end if 10s
  
  if(Barbell.fiveCheck.isSelected()){ 
   while(target >= fives.getWeight()){
    target -= fives.getWeight();
    fives.addOne();
    totalWeight += fives.getWeight();
   }// end add 5s
  }//end if 5s
  
  if(Barbell.twoPointFiveCheck.isSelected()){ 
   while(target >= twoPointFives.getWeight()){
    target -= twoPointFives.getWeight();
    twoPointFives.addOne();
    totalWeight += twoPointFives.getWeight();
   }// end add 2.5s
  }//end if 2.5s
 }//end addPlates
}//end Bar