package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Draw triangles with modeling transformations. Modified from
 * <code>model.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/ModelTransform.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/ModelTransform.help">
 * help output</a>
 */
public class ModelTransform extends gnu.x11.extension.glx.Application {
  public ModelTransform (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "modeling transformation",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
    init_window (500, 500);

    gl.shade_model (GL.FLAT);
  }


  private void draw_triangle () {
   gl.begin (GL.LINE_LOOP);
   gl.vertex2f (0.0f, 25.0f);
   gl.vertex2f (25.0f, -25.0f);
   gl.vertex2f (-25.0f, -25.0f);
   gl.end ();
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);

    // original triangle (solid line)
    gl.load_identity ();
    draw_triangle ();

    // translated triangle (widely-dotted line)
    gl.enable (GL.LINE_STIPPLE);
    gl.line_stipple (1, 0xf0f0);
    gl.load_identity ();
    gl.translatef (-20.0f, 0.0f, 0.0f);
    draw_triangle ();

    // scaled triangle (dotted line)
    gl.line_stipple (1, 0xf00f);
    gl.load_identity ();
    gl.scalef (1.5f, 0.5f, 1.0f);
    draw_triangle ();

    // rotated triangle (closely-dotted line)
    gl.line_stipple (1, 0x8888);
    gl.load_identity ();
    gl.rotatef (90.0f, 0.0f, 0.0f, 1.0f);
    draw_triangle ();
    gl.disable (GL.LINE_STIPPLE);

    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();

    double wh = (float) width / (float) height;
    double hw = (float) height / (float) width;
    
    if (width <= height)
      gl.ortho (-50.0, 50.0, -50.0*hw, 50.0*hw, -1.0, 1.0);
    else
      gl.ortho (-50.0*wh, 50.0*wh, -50.0, 50.0, -1.0, 1.0);

    gl.matrix_mode (GL.MODELVIEW);
  }


  public static void main (String [] args) {
    new ModelTransform (args).exec ();
  }
}
