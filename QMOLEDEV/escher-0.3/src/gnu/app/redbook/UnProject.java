package gnu.app.redbook;

import gnu.x11.extension.glx.GL;
import gnu.x11.Input;


/**
 * Un-project window coordinates. Modified from <code>unproject.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/UnProject.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/UnProject.output">
 * text output</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/UnProject.help">
 * help output</a>
 */
public class UnProject extends gnu.x11.extension.glx.Application {
  public UnProject (String [] args) {
    super (args, BUTTON_PRESS_BIT | RESIZE_BIT);

    about ("0.1", "un-project window coordinates",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
    init_window (500, 500);
  }


  protected void handle_button (int button, int state, int x, int y) {
    if (button == Input.BUTTON2) exit ();
    if (button != Input.BUTTON1) return;

    int [] viewport = gl.integerv (GL.VIEWPORT);
    double [] modelview = gl.doublev (GL.MODELVIEW_MATRIX);
    double [] projection = gl.doublev (GL.PROJECTION_MATRIX);

    int real_y = viewport [3] - y - 1;
    double [] world0 = glu.un_project (x, real_y, 0.0, modelview,
      projection, viewport);
    double [] world1 = glu.un_project (x, real_y, 1.0, modelview,
      projection, viewport);

    System.out.println ("Screen coord is (" + x + ", " + real_y + ")");
    System.out.println ("World coord at z=0.0 is ("
      + world0 [0] + ", " + world0 [1] + ", " + world0 [2] + ")");
    System.out.println ("World coord at z=1.0 is ("
      + world1 [0] + ", " + world1 [1] + ", " + world1 [2] + ")");
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);
    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();   
    double wh = (float) width / (float) height;
    glu.perspective (45.0,  wh, 1.0, 100.0);
    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
  }


  public static void main (String [] args) {
    new UnProject (args).exec ();
  }
}
