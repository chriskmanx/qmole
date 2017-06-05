package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Draw a wireframe cube. To demonstrate a single modeling transformation,
 * {@link GL#scalef} and a single viewing transformation, and {@link
 * gnu.x11.extension.glx.GLU#look_at}. Modified from <code>cube.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Cube.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Cube.help">
 * help output</a>
 */
public class Cube extends gnu.x11.extension.glx.Application {
  public Cube (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "wireframe cube",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
    init_window (500, 500);

    gl.shade_model (GL.FLAT);
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);

    gl.load_identity ();
    glu.look_at (0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
    gl.scalef (1.0f, 2.0f, 1.0f);
    glut.wire_cube (1.0f);

    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();   
    gl.frustum (-1.0, 1.0, -1.0, 1.0, 1.5, 20.0);
    gl.matrix_mode (GL.MODELVIEW);
  }


  public static void main (String [] args) {
    new Cube (args).exec ();
  }
}
