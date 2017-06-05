import java.awt.*;
import javax.swing.*;

public class Gui {

    public static void main(String[] args){
        notify("", "", "", 0);
    }

    public static void notify(String line1, String line2, String imagepath, int style){
        JFrame notification = new JFrame();
        JPanel main = new JPanel();
        JLabel notifyline1 = new JLabel();
        notifyline1.setText("test");
        main.add(notifyline1);
        notification.add(main);
        notification.setSize(new Dimension(320,64));
        notification.setLocationRelativeTo(null);
        notification.setUndecorated(true);
        notification.setVisible(true);
    }
}
