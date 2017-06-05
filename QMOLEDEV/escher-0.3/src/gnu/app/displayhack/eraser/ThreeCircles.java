package gnu.app.displayhack.eraser;


/**
 * Erase by three circles. 
 *
 * <p>Modified from <code>three_circle_wipe</code> of <code>erase.c</code>
 * in <a href="http://www.jwz.org/xscreensaver/">xscreensaver</a> by
 * Johannes Keukelaar <johannes@nada.kth.se>.
 */
public class ThreeCircles extends Eraser {
  public ThreeCircles () { super ("three-circle-wipe"); }


  public void erase (gnu.app.displayhack.DisplayHack hack) {
    gnu.x11.Window window = hack.window;

    /* Minimum radius = L/sqrt(2) ~= 0.707*L,
     *   where L = max(width, heigth)
     *         sqrt(2) = 1.414213562373095
     *
     * Take radius = L for faster (easier) computation.
     */
    int radius = Math.max (window.width, window.height);

    int full = 360 * 64;
    int third = full / 3;
    int start = hack.random.nextInt (full);

    /* 200 = speed in {@link CircleWipe}
     * 3 = three circles
     * 2 = converge from two sides
     */
    int delta = (full/200/3/2) * hack.eraser_delta;

    /* 1 to offset rounding error
     * 3 = three circles
     * 2 = converge from two sides
     */
    int step_count = 1 + full/delta/3/2;

   
    for (int i=0; i<step_count; i++) {
      // first circle
      window.fill_arc (hack.display.default_gc, 
        window.width/2 - radius, window.height/2 - radius,
        2*radius, 2*radius, (start+i*delta) % full, delta);
      window.fill_arc (hack.display.default_gc, 
        window.width/2 - radius, window.height/2 - radius,
        2*radius, 2*radius, (start-i*delta) % full, delta);
      
      // second circle
      window.fill_arc (hack.display.default_gc, 
        window.width/2 - radius, window.height/2 - radius,
        2*radius, 2*radius, (start+third+i*delta) % full, delta);
      window.fill_arc (hack.display.default_gc, 
        window.width/2 - radius, window.height/2 - radius,
        2*radius, 2*radius, (start+third-i*delta) % full, delta);

      // third circle
      window.fill_arc (hack.display.default_gc, 
        window.width/2 - radius, window.height/2 - radius,
        2*radius, 2*radius, (start+2*third+i*delta) % full, delta);
      window.fill_arc (hack.display.default_gc, 
        window.width/2 - radius, window.height/2 - radius,
        2*radius, 2*radius, (start+2*third-i*delta) % full, delta);

      if (sleep (hack)) return;
    }
    
    hack.sleep (hack.delay/2);  // before next screen
  }
}
