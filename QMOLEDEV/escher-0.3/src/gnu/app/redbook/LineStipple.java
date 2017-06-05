package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Do line stipple. Modified from <code>lines.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/LineStipple.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/LineStipple.help">
 * help output</a>
 */
public class LineStipple extends gnu.x11.extension.glx.Application {
  public LineStipple (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "line stipple",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
    init_window (400, 150);

    gl.shade_model (GL.FLAT);
  }


  private void draw_line (float x1, float y1, float x2, float y2) {
    gl.begin (GL.LINES);
    gl.vertex2f (x1, y1);
    gl.vertex2f (x2, y2);
    gl.end ();
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);
    gl.enable (GL.LINE_STIPPLE);


    // first row (three different lines)
    gl.line_stipple (1, 0x0101); // dotted
    draw_line (50.0f, 125.0f, 150.0f, 125.0f);
    gl.line_stipple (1, 0x00ff); // dashed
    draw_line (150.0f, 125.0f, 250.0f, 125.0f);
    gl.line_stipple (1, 0x1c47); // dashed/dot/dashed
    draw_line (250.0f, 125.0f, 350.0f, 125.0f);


    // second row (three different wide lines)
    gl.line_width (5.0f);
    gl.line_stipple (1, 0x0101); // dotted
    draw_line (50.0f, 100.0f, 150.0f, 100.0f);
    gl.line_stipple (1, 0x00ff); // dashed
    draw_line (150.0f, 100.0f, 250.0f, 100.0f);
    gl.line_stipple (1, 0x1c47); // dashed/dot/dashed
    draw_line (250.0f, 100.0f, 350.0f, 100.0f);
    gl.line_width (1.0f);


    // third row (six lines connected)
    gl.line_stipple (1, 0x1c47); // dashed/dot/dashed
    gl.begin (GL.LINE_STRIP);
    for (int i=0; i<7; i++) 
      gl.vertex2f (50.0f + (float) i * 50.0f, 75.0f);
    gl.end ();


    // fourth row (six lines independent)
    for (int i=0; i<6; i++) {
      float x1 = 50.0f + (float) i * 50.0f;
      float x2 = 50.0f + (float) (i+1) * 50.0f;
      draw_line (x1, 50.0f, x2, 50.0f);
    }

   
    // fifth row (one line with repeat factor = 5)
    gl.line_stipple (5, 0x1c47); // dashed/dot/dashed
    draw_line (50.0f, 25.0f, 350.0f, 25.0f);   

 
    gl.disable (GL.LINE_STIPPLE);
    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();
    glu.ortho_2d (0.0, (double) width, 0.0, (double) height);
  }


  public static void main (String [] args) {
    new LineStipple (args).exec ();
  }
}
