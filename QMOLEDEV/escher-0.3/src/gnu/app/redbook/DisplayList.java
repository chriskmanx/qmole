package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Draw triangles using display list. To demonstrate how to make and
 * execute a display list, and how attributes and current matrix are
 * changed. Modified from <code>list.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/DisplayList.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/DisplayList.help">
 * help output</a>
 */
public class DisplayList extends gnu.x11.extension.glx.Application {
  private int display_list;


  public DisplayList (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "display list",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
    init_window (650, 50);

    gl.shade_model (GL.FLAT);
    init_triangle ();
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);
    gl.color3f (0.0f, 1.0f, 0.0f); // green

    for (int i=0; i<10; i++) gl.call_list (display_list);
    
    /* Note that the following line is drawn with red color instead of
     * green, and drawn after the triangles instead of origin. It is
     * because executing a display list modify attributes (such as color)
     * as well as current matrix (such as translation).
     */
    gl.begin (GL.LINES);
    gl.vertex2f (0.0f, 0.5f);
    gl.vertex2f (15.0f, 0.5f);
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
      glu.ortho_2d (0.0, 2.0, -0.5*hw, 1.5*hw);
    else
      glu.ortho_2d (0.0, 2.0*wh, -0.5, 1.5);

    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
  }


  private void init_triangle () {
    display_list = gl.gen_lists (1);

    gl.new_list (display_list, GL.COMPILE);
    gl.color3f (1.0f, 0.0f, 0.0f);

    gl.begin (GL.TRIANGLES);
    gl.vertex2f (0.0f, 0.0f);
    gl.vertex2f (1.0f, 0.0f);
    gl.vertex2f (0.0f, 1.0f);
    gl.end ();

    gl.translatef (1.5f, 0.0f, 0.0f);
    gl.end_list ();
  }


  public static void main (String [] args) {
    new DisplayList (args).exec ();
  }
}
