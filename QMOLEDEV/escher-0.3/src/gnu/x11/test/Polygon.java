package gnu.x11.test;

import gnu.x11.Drawable;
import gnu.x11.Point;


/**
 * Test polygon drawings. 
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Polygon.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Polygon.help">
 * help output</a>
 */
public class Polygon extends Graphics {
  public Polygon (String [] args) { 
    super (args, 256, 256);

    about ("0.1", "test polygon drawings",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
  }


  public void paint () {
    window.rectangle (display.default_gc, 5, 5, window.width-10, 
      window.height-10, false);
    
    window.rectangle (display.default_gc, 10, 10, window.width-20, 
      window.height-20, false);
    

    Point [] points = {
      new Point (125, 50),
      new Point (150, 75),
      new Point (137, 100),
      new Point (113, 100),
      new Point (100, 75)
      };

    window.fill_poly (display.default_gc, points, Drawable.CONVEX,
                      Drawable.ORIGIN);
    display.flush ();
  }


  public static void main (String [] args) { 
    new Polygon (args).exec ();
  }
}
