import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

class Test2{
  
  public Test2(){
    JFileChooser chooser = new JFileChooser();
    FileNameExtensionFilter filter = new FileNameExtensionFilter(
                                                                 "JPG & GIF Images", "jpg", "gif");
    chooser.setFileFilter(filter);
    int returnVal = chooser.showOpenDialog(null);
    if(returnVal == JFileChooser.APPROVE_OPTION) {
      System.out.println("You chose to open this file: " +
                         chooser.getSelectedFile().getName());
    }
  }
  
  public static void main(String[] args){
    new Test2();
  }
}