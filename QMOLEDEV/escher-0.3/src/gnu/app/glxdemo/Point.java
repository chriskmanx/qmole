package gnu.app.glxdemo;

import gnu.x11.extension.glx.GL;
import gnu.x11.keysym.Misc;


/**
 * Draw a point with various attributes. moothing, press 's' or 'S'. 
 * Modified from <code>point.c</code> in <a href=
 * "http://trant.sgi.com/opengl/examples/samples/samples.html">
 * opengl sample</a> by SGI.
 *
 * <ul>
 * <li>To toggle stippling, press 'p' or 'P'.
 * <li>To toggle 
 * <li>To increase size, press PAGE-UP.
 * <li>To decrease size, press PAGE-DOWN.
 * <li>To move left, press LEFT.
 * <li>To move right, press RIGHT.
 * <li>To move up, press UP.
 * <li>To move down, press DOWN.
 * </ul>
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/Point.gif">
 * screenshot 6</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/Point.help">
 * help output</a>
 */
public class Point extends gnu.x11.extension.glx.Application {
  private static final float DELTA = 1.0f;


  private float [] position = {1.0f, 1.0f, 0.0f};
  private int point_size = 3;
  private boolean smoothing;


  public Point (String [] args) {
    super (args, KEYBOARD_BIT | RESIZE_BIT);

    about ("0.1", "point",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo toggle smoothing, press 's' or 'S'."
      + "\nTo increase size, press PAGE-UP."
      + "\nTo decrease size, press PAGE-DOWN."
      + "\nTo move left, press LEFT."
      + "\nTo move right, press RIGHT."
      + "\nTo move up, press UP."
      + "\nTo move down, press DOWN.");

    if (help_option) return;
    init_window (300, 300);

    gl.blend_func (GL.SRC_ALPHA, GL.ZERO);
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);

    //-- cross

    gl.color3f (1.0f, 1.0f, 0.0f);
    gl.begin (GL.LINE_STRIP);
    gl.vertex2f (-window.width/2, 0);
    gl.vertex2f (window.width/2, 0);
    gl.end ();

    gl.begin (GL.LINE_STRIP);
    gl.vertex2f (0, -window.height/2);
    gl.vertex2f (0, window.height/2);
    gl.end ();


    //-- point with attributes
    gl.capability (GL.BLEND, smoothing);
    gl.capability (GL.POINT_SMOOTH, smoothing);
    gl.point_size (point_size);
    gl.color3f (1.0f, 0.0f, 0.0f);
    gl.begin (GL.POINTS);
    gl.vertex3fv (position);
    gl.end ();

    //-- point without attributes
    gl.disable (GL.POINT_SMOOTH);
    gl.point_size (1);
    gl.color3f (0.0f, 1.0f, 0.0f);
    gl.begin (GL.POINTS);
    gl.vertex3fv (position);
    gl.end ();
       
    gl.swap_buffers (window);
  }


  protected void handle_keyboard (int key, int state, int x, int y) {
    switch (key) {
    case 's':                   // fall through
    case 'S': smoothing = !smoothing; break;
    case Misc.PAGE_UP: point_size++; break;
    case Misc.PAGE_DOWN: point_size--; break;
    case Misc.LEFT: position [0] -= DELTA; break;
    case Misc.RIGHT: position [0] += DELTA; break;
    case Misc.UP: position [1] += DELTA; break;
    case Misc.DOWN: position [1] -= DELTA; break;      
    default: return;
    }

    mark_window_dirty ();
  }

  
  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();   
    glu.ortho_2d (-width/2, width/2, -height/2, height/2);
    gl.matrix_mode (GL.MODELVIEW);
  }


  public static void main (String [] args) {
    new Point (args).exec ();
  }
}
