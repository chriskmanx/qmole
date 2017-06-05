package gnu.app.displayhack;


/** 
 * Draw tacky 70s basement wall panelling. It subdivides and colors
 * rectangles randomly. It looks kind of like Brady-Bunch-era rec-room wall
 * paneling. (Raven says: "this screensaver is ugly enough to peel paint.")
 * 
 * <p>Modified from <code>deco.c</code> in <a href=
 * "http://www.jwz.org/xscreensaver/">xscreensaver</a>
 * by Jamie Zawinski <jwz@jwz.org>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/displayhack/Deco.gif">
 * screenshot 8</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/displayhack/Deco.help">
 * help output</a>
 */
public class Deco extends DisplayHack {
  public int max_depth, min_size;


  public Deco (String [] args) { 
    super (args, false, false, false, 64, 1000);

    max_depth = option.intt ("max-depth", 
      "maximum level of recursion", 12);
    min_size = option.intt ("min-size", 
      "mimimum size of width and height", 20);

    about ("0.1", "draw tacky 70s basement wall panelling",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
  }


  public void deco (int x, int y, int width, int height, int depth) {
    if (width < min_size || height < min_size
      || random.nextInt (max_depth) > depth) { // stop recursion
    
      gc.set_foreground (random_color ());
      window.rectangle (gc, x, y, width, height, true);

      // draw border to look nice
      window.rectangle (display.default_gc, x, y, width, height, false);

    } else if (random.nextBoolean ()) {
      /* A rounding bug in original implementation.
       *
       * Note rounding error in width/2 + widht/2 != width!!!
       * Use <code>width-width/2</code> to get ceiling.
       */

      // left-right split
      deco (x, y, width/2, height, depth-1);
      deco (x+width/2, y, width-width/2, height, depth-1);

    } else {
      // top-bottom split
      deco (x, y, width, height/2, depth-1);
      deco (x, y+height/2, width, height-height/2, depth-1);
    }
  }



  public void paint () {
    deco (0, 0, window.width, window.height, max_depth);
  }


  public static void main (String [] args) { 
    new Deco (args).exec (); 
  }
}
