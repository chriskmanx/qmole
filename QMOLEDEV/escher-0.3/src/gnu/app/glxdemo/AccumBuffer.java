package gnu.app.glxdemo;

import gnu.x11.extension.glx.GL;


/**
 * Draw with accumulation buffer. To fill rectangles, press '1'. To draw
 * lines, press '2'. Modified from <code>accum.c</code> in <a href=
 * "http://trant.sgi.com/opengl/examples/samples/samples.html">
 * opengl sample</a> by SGI.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/AccumBuffer.gif">
 * screenshot 2</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/AccumBuffer.help">
 * help output</a>
 */
public class AccumBuffer extends gnu.x11.extension.glx.Application {
  private int rectangle1, rectangle2;


  public AccumBuffer (String [] args) {
    super (args, KEYBOARD_BIT | RESIZE_BIT);

    about ("0.1", "accumulation buffer",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo fill rectangles, press '1'."
      + "\nTo draw lines, press '2'.");

    if (help_option) return;

    visual_config.set_accum_rgb_size (1);
    visual_config.set_double_buffer ();
    init_window (300, 300);

    init_rectangles ();
  }


  protected void handle_expose () {
    gl.rectf (-1.0f, -1.0f, 1.0f, 0.0f);

    gl.push_matrix ();
    gl.scalef (0.8f, 0.8f, 1.0f);

    gl.clear (GL.COLOR_BUFFER_BIT);
    gl.call_list (rectangle1);
    gl.accum (GL.LOAD, 0.5f);

    gl.clear (GL.COLOR_BUFFER_BIT);
    gl.call_list (rectangle2);
    gl.accum (GL.ACCUM, 0.5f);

    gl.accum (GL.RETURN, 1.0f);
    gl.pop_matrix ();
    gl.swap_buffers (window);
  }


  protected void handle_keyboard (int key, int state, int x, int y) {
    switch (key) {
    case '1': gl.polygon_mode (GL.FRONT_AND_BACK, GL.FILL); break;
    case '2': gl.polygon_mode (GL.FRONT_AND_BACK, GL.LINE); break;
    default: return;
    }

    mark_window_dirty ();
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();
    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
  }


  private void init_rectangles () {
    rectangle1 = gl.gen_lists (1);
    gl.new_list (rectangle1, GL.COMPILE);
    gl.color3f (1.0f, 0.0f, 0.0f);
    gl.rectf (-1.0f, -1.0f, 1.0f, 0.0f);
    gl.end_list ();

    rectangle2 = gl.gen_lists (1);
    gl.new_list (rectangle2, GL.COMPILE);
    gl.color3f (0.0f, 1.0f, 0.0f);
    gl.rectf (0.0f, -1.0f, 1.0f, 1.0f);
    gl.end_list ();
  }


  public static void main (String [] args) {
    new AccumBuffer (args).exec ();
  }
}
