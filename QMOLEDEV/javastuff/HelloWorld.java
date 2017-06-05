import java.awt.*;
import java.awt.event.*;
import javax.swing.JLabel;

public class HelloWorld extends Frame {
   public static void main (String [] argv){
      HelloWorld h = new HelloWorld();
   }
   public HelloWorld(){
     setTitle("Hello World!");
     setSize(150, 125);
     Panel hello = new Panel();
     add("Center", hello);
  
     Button button = new Button("OK");
     add("South", button);

     Button xbutton = new Button("Hello World");
     add("North",xbutton);

     button.addActionListener(new ActionListener() {
         public void actionPerformed(ActionEvent e) {
            System.exit(0);
         }
       });
     setVisible(true);
     //int width = hello.getWidth();
     //int height = hello.getHeight();
     //Graphics g = hello.getGraphics();
     //g.drawString("Hello World!", width/2 - 25, height/2);
     addWindowListener(new WindowAdapter(){
          public void windowClosing(WindowEvent e){ 
              System.exit(0);
          }
     });
   }
}
