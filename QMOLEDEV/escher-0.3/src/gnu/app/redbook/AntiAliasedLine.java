package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Draw anti-aliased lines. To rotate the diagonal lines, press 'r' or 'R'. 
 * Modified from <code>aargb.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/AntiAliasedLine.gif">
 * screenshot 6</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/AntiAliasedLine.output">
 * text output</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/AntiAliasedLine.help">
 * help output</a>
 */
public class AntiAliasedLine extends gnu.x11.extension.glx.Application {
  private int rotate_angle;


  public AntiAliasedLine (String [] args) {
    super (args, RESIZE_BIT | KEYBOARD_BIT);

    about ("0.1", "anti-aliased lines",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo rotate lines, press 'r' or 'R'.");

    if (help_option) return;
    init_window (200, 200);

    System.out.println ("GL_LINE_WIDTH_GRANULARITY = "
      + gl.floatv (GL.LINE_WIDTH_GRANULARITY) [0]);

    float[] fv = gl.floatv (GL.LINE_WIDTH_RANGE);
    System.out.println ("GL_LINE_WIDTH_RANGE = "
      + fv [0] + " to " + fv [1]);

    gl.enable (GL.LINE_SMOOTH);
    gl.enable (GL.BLEND);
    gl.blend_func (GL.SRC_ALPHA, GL.ONE_MINUS_SRC_ALPHA);
    gl.hint (GL.LINE_SMOOTH_HINT, GL.DONT_CARE);
    gl.line_width (1.5f);
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);

    // green line
    gl.color3f (0.0f, 1.0f, 0.0f);
    gl.push_matrix ();
    gl.rotatef (-rotate_angle, 0.0f, 0.0f, 0.1f);
    gl.begin (GL.LINES);
    gl.vertex2f (-0.5f, 0.5f);
    gl.vertex2f (0.5f, -0.5f);
    gl.end ();
    gl.pop_matrix ();

    // blue line
    gl.color3f (0.0f, 0.0f, 1.0f);
    gl.push_matrix ();
    gl.rotatef (rotate_angle, 0.0f, 0.0f, 0.1f);
    gl.begin (GL.LINES);
    gl.vertex2f (0.5f, 0.5f);
    gl.vertex2f (-0.5f, -0.5f);
    gl.end ();
    gl.pop_matrix ();
    
    gl.swap_buffers (window);
  }


  protected void handle_keyboard (int key, int state, int x, int y) {
    if (key == 'r' || key == 'R') {
      rotate_angle = (rotate_angle + 2) % 360;
      mark_window_dirty ();
    }
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();

    double wh = (float) width / (float) height;
    double hw = (float) height / (float) width;
    
    if (width <= height)
      gl.ortho (-1.0, 1.0, -1.0*hw, 1.0*hw, -1.0, 1.0);
    else
      gl.ortho (-1.0*wh, 1.0*wh, -1.0, 1.0, -1.0, 1.0);

    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
  }


  public static void main (String [] args) {
    new AntiAliasedLine (args).exec ();
  }
}
