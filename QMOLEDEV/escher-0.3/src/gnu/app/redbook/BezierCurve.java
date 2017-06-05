package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Draw a Bezier curve. Modified from <code>bezcurve.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/BezierCurve.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/BezierCurve.help">
 * help output</a>
 */
public class BezierCurve extends gnu.x11.extension.glx.Application {
  private static final double [] CONTROL_POINTS = {
    -4.0f, -4.0f, 0.0f,
    -2.0f, 4.0f, 0.0f,
    2.0f, -4.0f, 0.0f,
    4.0f, 4.0f, 0.0f
  };


  public BezierCurve (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "bezier curve",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
    init_window (500, 500);

    gl.enable (GL.MAP1_VERTEX_3);
    gl.shade_model (GL.FLAT);
    gl.map1d (GL.MAP1_VERTEX_3, 0.0f, 1.0f, 3, 4, CONTROL_POINTS);
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);

    // bezier curve
    gl.begin (GL.LINE_STRIP);
    for (int i=0; i<=30; i++)
      gl.eval_coord1f (i/30.0f);
    gl.end ();

    // control points
    gl.point_size (5.0f);
    gl.color3f (1.0f, 1.0f, 0.0f);
    gl.begin (GL.POINTS);
    for (int i=0; i<4; i++) 
      gl.vertex3d (CONTROL_POINTS [3*i], 
        CONTROL_POINTS [3*i+1], 
        CONTROL_POINTS [3*i+2]);
    gl.end ();

    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();

    double wh = (float) width / (float) height;
    double hw = (float) height / (float) width;
    
    if (width <= height)
      gl.ortho (-5.0, 5.0, -5.0*hw, 5.0*hw, -5.0, 5.0);
    else
      gl.ortho (-5.0*wh, 5.0*wh, -5.0, 5.0, -5.0, 5.0);

    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
  }


  public static void main (String [] args) {
    new BezierCurve (args).exec ();
  }
}
