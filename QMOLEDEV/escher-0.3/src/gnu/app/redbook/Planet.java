package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Sun and planet. To demonstrate how to composite modeling transformations
 * to draw translated and rotated models. Modified from
 * <code>planet.c</code>.
 *
 * <ul>
 * <li>To rotate counter-clockwise about planet, press 'd'.
 * <li>To rotate clockwise about planet, press 'D'.
 * <li>To rotate counter-clockwise about planet, press 'y'.
 * <li>To rotate clockwise about planet, press 'Y'.
 * </ul>
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Planet.gif">
 * screenshot 4</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Planet.help">
 * help output</a>
 */
public class Planet extends gnu.x11.extension.glx.Application {
  private int day, year;


  public Planet (String [] args) {
    super (args, KEYBOARD_BIT | RESIZE_BIT);

    about ("0.1", "sun and planet",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo rotate counter-clockwise about planet, press 'd'."
      + "\nTo rotate clockwise about planet, press 'D'."
      + "\nTo rotate counter-clockwise about planet, press 'y'."
      + "\nTo rotate clockwise about planet, press 'Y'.");

    if (help_option) return;

    visual_config.set_double_buffer ();
    init_window (500, 500);

    gl.shade_model (GL.FLAT);
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);
    gl.color3f (1.0f, 1.0f, 1.0f);
    gl.push_matrix ();

    // sun
    glut.wire_sphere (1.0, 20, 16);

    // planet
    gl.rotatef (year, 0.0f, 1.0f, 0.0f);
    gl.translatef (2.0f, 0.0f, 0.0f);
    gl.rotatef (day, 0.0f, 1.0f, 0.0f);
    glut.wire_sphere (0.2, 10, 8);

    gl.pop_matrix ();
    gl.swap_buffers (window);
  }


  protected void handle_keyboard (int key, int state, int x, int y) {
    switch (key) {
    case 'd': day = (day + 10) % 360; break;
    case 'D': day = (day - 10) % 360; break;
    case 'y': year = (year + 5) % 360; break;
    case 'Y': year = (year - 5) % 360; break;
    default: return;
    }

    mark_window_dirty ();
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();   
    double wh = (float) width / (float) height;
    glu.perspective (60.0,  wh, 1.0, 20.0);
    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();   
    glu.look_at (0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
  }


  public static void main (String [] args) {
    new Planet (args).exec ();
  }
}
