package gnu.app.displayhack;


/** 
 * Simulate ink-blot patterns. It draws random patterns reminiscent of the
 * psychological test of same name.
 *
 * <p>Modified from <code>rorschach.c</code> in <a href=
 * "http://www.jwz.org/xscreensaver/">xscreensaver</a>
 * by Jamie Zawinski <jwz@jwz.org>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/displayhack/Rorschach.gif">
 * screenshot 8</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/displayhack/Rorschach.help">
 * help output</a>
 */
public class Rorschach extends DisplayHack {
  public int depth, offset, step_size;
  public long draw_delay;
  public boolean x_symmetry, y_symmetry;


  public Rorschach (String [] args) { 
    super (args, true, true, true, 16, 2000);

    depth = option.intt ("depth", "level of details", 10000);
    draw_delay = option.longg ("draw-delay", 
      "delay between iterations in millis", 10);
    offset = option.intt ("offset", "compactness of pattern", 4);
    int step_count = option.intt ("step-count", 
      "total number of steps", 50);
    x_symmetry = option.booleann ("x-symmetry", 
      "if symmetry along x-axis", true);
    y_symmetry = option.booleann ("y-symmetry", 
      "if symmetry along y-axis", false);

    about ("0.1", "simulate ink-blot patterns",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    step_size = depth / step_count;
  }


  public void paint () {
    gc.set_foreground (random_color ());    
    
    int xlim = window.width;
    int ylim = window.height;

    int x = xlim/2;
    int y = ylim/2;

    for (int i=0; i<depth; i++) {
      x += random.nextInt (1 + 2*offset) - offset;
      y += random.nextInt (1 + 2*offset) - offset;
      window.point (gc, x, y);
      
      if (x_symmetry) window.point (gc, xlim-x, y);
      if (y_symmetry) window.point (gc, x, ylim-y);
      if (x_symmetry && y_symmetry) window.point (gc, xlim-x, ylim-y);

      if (step_size != 0 && i % step_size == 0)
        if (sleep (draw_delay)) return;
    }
  }


  public static void main (String [] args) { 
    new Rorschach (args).exec (); 
  }
}
