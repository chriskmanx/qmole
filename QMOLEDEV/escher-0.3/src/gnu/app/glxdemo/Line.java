package gnu.app.glxdemo;

import gnu.x11.extension.glx.GL;
import gnu.x11.keysym.Misc;


/**
 * Draw a line with various attributes. moothing, press 's' or 'S'. 
 * Modified from <code>line.c</code> in <a href=
 * "http://trant.sgi.com/opengl/examples/samples/samples.html"> opengl
 * sample</a> by SGI.
 *
 * <ul>
 * <li>To toggle stippling, press 'p' or 'P'.
 * <li>To toggle 
 * <li>To increase size, press UP.
 * <li>To decrease size, press DOWN.
 * </ul>
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/Line.gif">
 * screenshot 8</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/Line.help">
 * help output</a>
 */
public class Line extends gnu.x11.extension.glx.Application {
  private static final float [] POINT_A = {-160.0f, 0.0f, 0.0f};
  private static final float [] POINT_B = {-130.0f, 0.0f, 0.0f};
  private static final float [] POINT_C = {-40.0f, -50.0f, 0.0f};
  private static final float [] POINT_D = {300.0f, 60.0f, 0.0f};


  private int draw_size = 1;
  private boolean stippling, smoothing;


  public Line (String [] args) {
    super (args, KEYBOARD_BIT | RESIZE_BIT);

    about ("0.1", "line",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo toggle stippling, press 'p' or 'P'."
      + "\nTo toggle smoothing, press 's' or 'S'."
      + "\nTo increase size, press UP."
      + "\nTo decrease size, press DOWN.");

    if (help_option) return;
    init_window (300, 300);

    gl.line_stipple (1, 0xf0e0);
    gl.blend_func (GL.SRC_ALPHA, GL.ONE);
  }


  private void draw_line () {
    gl.line_width (draw_size);    
    gl.color3f (1.0f, 1.0f, 0.0f);

    gl.begin (GL.LINE_STRIP);
    gl.vertex3fv (POINT_A);
    gl.vertex3fv (POINT_B);
    gl.end ();
  }


  private void draw_point () {
    gl.point_size (1);
    gl.color3f (0.0f, 1.0f, 0.0f);

    gl.begin (GL.POINTS);
    gl.vertex3fv (POINT_A);
    gl.vertex3fv (POINT_B);
    gl.end ();
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);
    gl.capability (GL.BLEND, smoothing);
    gl.capability (GL.LINE_SMOOTH, smoothing);
    gl.capability (GL.LINE_STIPPLE, stippling);

    gl.push_matrix ();

    for (int i=0; i<360; i+=5) {
      gl.rotatef (5.0f, 0.0f, 0.0f, 1.0f);
      draw_line ();
      draw_point ();
    }

    gl.pop_matrix ();
    gl.swap_buffers (window);
  }


  protected void handle_keyboard (int key, int state, int x, int y) {
    switch (key) {
    case 'p':                   // fall through
    case 'P': stippling = !stippling; break;
    case 's':                   // fall through
    case 'S': smoothing = !smoothing; break;
    case Misc.UP: draw_size++; break;

    case Misc.DOWN: 
      draw_size--;
      if (draw_size < 1) draw_size = 1;
      break;
      
    default: return;
    }

    mark_window_dirty ();
  }

  
  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();   
    glu.ortho_2d (-175, 175, -175, 175);
    gl.matrix_mode (GL.MODELVIEW);
  }


  public static void main (String [] args) {
    new Line (args).exec ();
  }
}
