package gnu.app.displayhack.eraser;


/**
 * Erase by a circle. 
 *
 * <p>Modified from <code>circle_wipe</code> of <code>erase.c</code> in <a
 * href="http://www.jwz.org/xscreensaver/">xscreensaver</a> by Johannes
 * Keukelaar <johannes@nada.kth.se>.
 */
public class Circle extends Eraser {
  public Circle () { super ("circle-wipe"); }


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
    int delta = (full/200) * hack.eraser_delta;
    int step_count = (int) Math.ceil (1 + full/delta);
    int start = hack.random.nextInt (full);

    // clockwise or counter-clockwise
    if (hack.random.nextBoolean ()) delta = -delta;


    for (int i=0; i<step_count; i++) {
      window.fill_arc (hack.display.default_gc, 
        window.width/2 - radius, window.height/2 - radius,
        2*radius, 2*radius, (start+i*delta) % full, delta);

      if (sleep (hack)) return;
    }
    
    hack.sleep (hack.delay/2);  // before next screen
  }
}
