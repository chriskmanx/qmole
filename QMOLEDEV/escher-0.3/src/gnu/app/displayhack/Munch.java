package gnu.app.displayhack;


/**
 * Munching squares. It performs the munching squares hack until killed. It
 * picks square size, position, and gravity randomly. It consists of drawing
 * Y = X XOR T for a range of X and T over and over until all the possible
 * combinations of X and T have come up. It was reportedly discovered by
 * Jackson Wright in 1962 and took 5 instructions of PDP-6 code.
 * 
 * <p>Modified from <code>munch.c</code> in <a href=
 * "http://www.jwz.org/xscreensaver/">xscreensaver</a> by Tim Showalter
 * <tjs@andrew.cmu.edu>. Cleaned up to used eraser.
 *
 * @see <a href=
 * "http://www.tuxedo.org/~esr/jargon/html/entry/munching-squares.html">
 * jargon file</a>
 *
 * @see <a href=
 * "http://www.inwap.com/pdp10/hbaker/hakmem/hacks.html#item146">
 * hakem</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/displayhack/Munch.gif">
 * screenshot 8</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/displayhack/Munch.help">
 * help output</a>
 */
public class Munch extends DisplayHack {
  public long draw_delay;
  public int square_count, min_size;
  public boolean xor, shift;


  public Munch (String [] args) { 
    super (args, true, true, false, 16, 2000);

    draw_delay = option.longg ("draw-delay", 
      "delay between iterations in millis", 10);
    min_size = option.intt ("min-size", 
      "minimum size of square", 64);
    shift = option.booleann ("shift",
      "shift factor = munch in random order", true);
    square_count = option.intt ("square-count", 
      "total number of squares", 5);
    xor = option.booleann ("xor",
      "use xor function to draw", true);

    about ("0.1", "munching squares",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    // TODO alpha composite (from RENDER/GLX) instead of xor?
    if (xor) gc.set_function (gnu.x11.GC.Values.XOR);
  }


  public void paint () {
    int window_size = Math.min (window.width, window.height);

    for (int i=0; i<square_count; i++) {
      int random_size = window_size <= min_size ? window_size
        : min_size + random.nextInt (window_size-min_size);

      /* Size of square must be a power of two, or filling will not be
       * completed.
       */
      int size = 1 << (int) (Math.log (random_size) / Math.log (2));


      // square always fully inside the window
      int origin_x = random_int (window.width - size);
      int origin_y = random_int (window.height - size);

      gc.set_foreground (random_color ());

      // shift factors
      int t0 = 0, x0 = 0, y0 = 0;
      if (shift) {
        t0 = random.nextInt (size);
        x0 = random.nextInt (size);
        y0 = random.nextInt (size);
      }      

      for (int t=0; t<size; t++) {
        for (int x=0; x<size; x++) {
          int y = x ^ ((t + t0) % size);

          int point_x = origin_x + (x + x0) % size;
          int point_y = origin_y + (y + y0) % size;
          
          window.point (gc, point_x, point_y);          
        }
        if (sleep (draw_delay)) return;
      }
    }
  }


  public static void main (String [] args) { 
    new Munch (args).exec (); 
  }
}
