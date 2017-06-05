package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Draw a torus. Modified from <code>torus.c</code>.
 *
 * <ul>
 * <li>To rotate about x-axis, press 'x' or 'X'.
 * <li>To rotate about y-axis, press 'y' or 'Y'.
 * <li>To reset, press 'r' or 'R'.
 * </ul>
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Torus.gif">
 * screenshot 4</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Torus.help">
 * help output</a>
 */
public class Torus extends gnu.x11.extension.glx.Application {
  private int display_list;


  public Torus (String [] args) {
    super (args, KEYBOARD_BIT | RESIZE_BIT);

    about ("0.1", "modeling transformation",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo rotate about x-axis, press 'x' or 'X'."
      + "\nTo rotate about y-axis, press 'y' or 'Y'."
      + "\nTo reset, press 'r' or 'R'.");

    if (help_option) return;
    init_window (200, 200);

    gl.shade_model (GL.FLAT);

    display_list = gl.gen_lists (1);
    gl.new_list (display_list, GL.COMPILE);
    draw_torus (8, 25);
    gl.end_list ();
  }


  private void draw_torus (int numc, int numt) {
    for (int i=0; i<numt; i++) {
      gl.begin (GL.QUAD_STRIP);

      for (int j=0; j<=numt; j++) {
        for (int k=1; k>=0; k--) {
          double s = (i + k) % numc + 0.5;
          double t = j % numt;
          double alpha = s * Math.PI * 2 / numc;
          double beta = t * Math.PI * 2 / numt;
          double p = 1 + 0.1 * Math.cos (alpha);
        
          double x = p * Math.cos (beta);
          double y = p * Math.sin (beta);
          double z = 0.1 * Math.sin (alpha);
          gl.vertex3f ((float) x, (float) y, (float) z);
        }
      }
      gl.end ();
    }
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);
    gl.call_list (display_list);
    gl.swap_buffers (window);
  }


  protected void handle_keyboard (int key, int state, int x, int y) {
    switch (key) {
    case 'x':                   // fall through
    case 'X': gl.rotatef (30.0f, 1.0f, 0.0f, 0.0f); break;
    case 'y':                   // fall through
    case 'Y': gl.rotatef (30.0f, 0.0f, 1.0f, 0.0f); break;
    case 'r':                   // fall through

    case 'R':
      gl.load_identity ();
      glu.look_at (0.0, 0.0, 10.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
      break;
    }

    mark_window_dirty ();
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();    
    double wh = (float) width / (float) height;
    glu.perspective (30.0, wh, 1.0, 100.0);
    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
    glu.look_at (0.0, 0.0, 10.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
  }


  public static void main (String [] args) {
    new Torus (args).exec ();
  }
}
