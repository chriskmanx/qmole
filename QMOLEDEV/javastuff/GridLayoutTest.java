import java.awt.GridLayout;

import javax.swing.JButton;
import javax.swing.JFrame;

public class GridLayoutTest {

    public static void main(String[] args) {
	JFrame.setDefaultLookAndFeelDecorated(true);
	JFrame frame = new JFrame("GridLayout Test");
	frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	frame.setLayout(new GridLayout(3, 2));
	frame.add(new JButton("Button 1"));
	frame.add(new JButton("Button 2"));
	frame.add(new JButton("Button 3"));
	frame.add(new JButton("Button 4"));
	frame.add(new JButton("Button 5"));
	frame.add(new JButton("Button 6"));
	frame.add(new JButton("Button 7"));
	frame.add(new JButton("Button 8"));
	frame.pack();
	frame.setVisible(true);
    }
}
