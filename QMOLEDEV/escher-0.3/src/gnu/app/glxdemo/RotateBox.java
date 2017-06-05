package gnu.app.glxdemo;

import gnu.x11.extension.glx.GL;
import gnu.x11.Input;


/**
 * Draw a colorful box for rotation. Modified from
 * <code>xlib\glxsimple.c</code> in <a href=
 * "ftp://ftp.sgi.com/opengl/opengl_for_x/">
 * glx sample</a> by Mark J. Kilgard.
 *
 * <ul>
 * <li>To rotate about x-axis, press BUTTON1.
 * <li>To rotate about y-axis, press BUTTON2.
 * <li>To rotate about z-axis, press BUTTON3.
 * </ul>
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/RotateBox.gif">
 * screenshot 6</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/RotateBox.help">
 * help output</a>
 */
public class RotateBox extends gnu.x11.extension.glx.Application {
  private float x_angle = 42.0f;
  private float y_angle = 82.0f;
  private float z_angle = 112.0f;


  public RotateBox (String [] args) {
    super (args, BUTTON_PRESS_BIT | RESIZE_BIT);

    about ("0.1", "box for rotation",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo rotate about x-axis, press BUTTON1."
      + "\nTo rotate about y-axis, press BUTTON2."
      + "\nTo rotate about z-axis, press BUTTON3.");

    if (help_option) return;

    visual_config.set_depth_size (1);
    init_window (300, 300);

    gl.enable (GL.DEPTH_TEST);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();
    gl.frustum (-1.0, 1.0, -1.0, 1.0, 1.0, 10.0);
    init_view ();
    init_box ();
  }


  protected void handle_button (int button, int state, int x, int y) {
    switch (button) {
    case Input.BUTTON1: x_angle += 10.0; break;
    case Input.BUTTON2: y_angle += 10.0; break;
    case Input.BUTTON3: z_angle += 10.0; break;
    default: return;
    }

    init_view ();
    mark_window_dirty ();
  }


  protected void handle_expose () {
    gl.call_list (1);
    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
  }


  private void init_box () {
    gl.new_list (1, GL.COMPILE);
    gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);

    gl.begin (GL.QUADS);

    // front face - green
    gl.color3f (0.0f, 0.7f, 0.1f);
    gl.vertex3f (-1.0f, 1.0f, 1.0f);
    gl.vertex3f (1.0f, 1.0f, 1.0f);
    gl.vertex3f (1.0f, -1.0f, 1.0f);
    gl.vertex3f (-1.0f, -1.0f, 1.0f);

    // back face - yellow
    gl.color3f (0.9f, 1.0f, 0.0f);
    gl.vertex3f (-1.0f, 1.0f, -1.0f);
    gl.vertex3f (1.0f, 1.0f, -1.0f);
    gl.vertex3f (1.0f, -1.0f, -1.0f);
    gl.vertex3f (-1.0f, -1.0f, -1.0f);

    // top side face - blue
    gl.color3f (0.2f, 0.2f, 1.0f);
    gl.vertex3f (-1.0f, 1.0f, 1.0f);
    gl.vertex3f (1.0f, 1.0f, 1.0f);
    gl.vertex3f (1.0f, 1.0f, -1.0f);
    gl.vertex3f (-1.0f, 1.0f, -1.0f);

    // bottom side face - red
    gl.color3f (0.7f, 0.0f, 0.1f);
    gl.vertex3f (-1.0f, -1.0f, 1.0f);
    gl.vertex3f (1.0f, -1.0f, 1.0f);
    gl.vertex3f (1.0f, -1.0f, -1.0f);
    gl.vertex3f (-1.0f, -1.0f, -1.0f);

    gl.end ();
    gl.end_list ();
  }


  private void init_view () {
    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
    gl.translatef (0.0f, 0.0f, -3.0f);
    gl.rotatef (x_angle, 0.1f, 0.0f, 0.0f);
    gl.rotatef (y_angle, 0.0f, 0.1f, 0.0f);
    gl.rotatef (z_angle, 0.0f, 0.0f, 1.0f);    
  }
   

  public static void main (String [] args) {
    new RotateBox (args).exec ();
  }
}
