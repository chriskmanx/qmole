package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Test clipping planes. Modified from <code>clip.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Clip.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Clip.help">
 * help output</a>
 */
public class Clip extends gnu.x11.extension.glx.Application {
  private static final double [] CLIP_PLANE0 = {0.0, 1.0, 0.0, 0.0};
  private static final double [] CLIP_PLANE1 = {1.0, 0.0, 0.0, 0.0};


  public Clip (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "clipping plane",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
    init_window (500, 500);

    gl.enable (GL.CLIP_PLANE0);
    gl.enable (GL.CLIP_PLANE1);
    gl.shade_model (GL.FLAT);
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);

    gl.color3f (1.0f, 1.0f, 1.0f);
    gl.push_matrix ();
    gl.translatef (0.0f, 0.0f, -5.0f);

    // clip lower half (y < 0)
    gl.clip_plane (GL.CLIP_PLANE0, CLIP_PLANE0);

    // clip left half (x < 0)
    gl.clip_plane (GL.CLIP_PLANE1, CLIP_PLANE1);

    gl.rotatef (90.0f, 1.0f, 0.0f, 0.0f);
    glut.wire_sphere (1.0, 20, 16);
    gl.pop_matrix ();

    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();   
    double wh = (float) width / (float) height;
    glu.perspective (60.0,  wh, 1.0, 20.0);
    gl.matrix_mode (GL.MODELVIEW);
  }


  public static void main (String [] args) {
    new Clip (args).exec ();
  }
}
