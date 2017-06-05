package gnu.app.displayhack.eraser;

import gnu.x11.Window;
import gnu.app.displayhack.DisplayHack;


/**
 * Erase by sliding lines.
 *
 * <p>Modified from <code>slide_lines</code> of <code>erase.c</code> in <a
 * href="http://www.jwz.org/xscreensaver/">xscreensaver</a> by Torbjorn
 * Andersson <torbjorn@dev.eurotime.se>.
 *
 * <p>Trivially added erase in y-direction support.
 */
public class SlideLines extends Eraser {
  /**
   * TODO
   * 1. accelerating slide?
   * 2. slide in any directions?
   */

  public static final int SLICE = 100;
  public static final int DELAY = 200;

  public SlideLines () { super ("slide-lines"); }


  public void erase (DisplayHack hack) {
    // no useless notify events
    hack.eraser_gc.set_graphics_exposures (false);

    if (hack.random.nextBoolean ())
      horizontal_erase (hack);
    else
      vertical_erase (hack);
    
    hack.sleep (hack.delay/2);  // before next screen
  }


  /** 
   * Erase in x-direction. Exactly the same as {@link
   * #vertical_erase(DisplayHack)} with x and y-related swapped.
   */
  public void horizontal_erase (DisplayHack hack) {
    Window window = hack.window;
    int dx = Math.max (1, window.width / DELAY * hack.eraser_delta);
    int dy = Math.max (1, window.height / SLICE * hack.eraser_delta);

    for (int x=0; x<window.width; x+=dx) {
      for (int y=0; y<window.height; y+=dy) {
        int from_x, to_x, clear_x;

        if ((y/dy) % 2 == 1) {
          from_x = x;
          to_x = x + dx;
          clear_x = x;

        } else {
          from_x = dx;
          to_x = 0;
          clear_x = window.width - x - dx;
        }
          
        window.copy_area (window, hack.eraser_gc, from_x, y, 
          window.width-x-dx, dy, to_x, y);
        window.clear_area (clear_x, y, dx, dy, false);
      }

      if (sleep (hack)) return;
    }
  }


  /** 
   * Erase in y-direction. Exactly the same as {@link
   * #horizontal_erase(DisplayHack)} with x and y-related swapped.
   */
  public void vertical_erase (DisplayHack hack) {
    Window window = hack.window;
    int dx = Math.max (1, window.width / SLICE * hack.eraser_delta);
    int dy = Math.max (1, window.height / DELAY * hack.eraser_delta);

    for (int y=0; y<window.height; y+=dy) {
      for (int x=0; x<window.width; x+=dx) {
        int from_y, to_y, clear_y;

        if ((x/dx) % 2 == 1) {
          from_y = y;
          to_y = y + dy;
          clear_y = y;

        } else {
          from_y = dy;
          to_y = 0;
          clear_y = window.height - y - dy;
        }
          
        window.copy_area (window, hack.eraser_gc, x, from_y,
          dx, window.height-y-dy, x, to_y);
        window.clear_area (x, clear_y, dx, dy, false);
      }

      if (sleep (hack)) return;
    }
  }
}
