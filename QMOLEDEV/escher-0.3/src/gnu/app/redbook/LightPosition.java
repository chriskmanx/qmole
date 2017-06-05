package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Test light position. To demonstrate when to issue lighting and
 * transformation commands to render a model with a light which is moved by
 * a modeling transformation (rotate or translate). The light position is
 * reset after the modeling transformation is called. The eye position does
 * not change. To rotate position, press 'r', 'R' or BUTTON1. A torus is
 * drawn using a grey material characteristic. A single light source
 * illuminates the object. Modified from <code>movelight.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/LightPosition.gif">
 * screenshot 12</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/LightPosition.help">
 * help output</a>
 */
public class LightPosition extends gnu.x11.extension.glx.Application {
  private static final float [] LIGHT_POSITION = {0.0f, 0.0f, 1.5f, 1.0f};

  private int rotate_angle;


  public LightPosition (String [] args) {
    super (args, BUTTON_PRESS_BIT | KEYBOARD_BIT | RESIZE_BIT);

    about ("0.1", "light position",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo rotate position, press 'r', 'R' or BUTTON1.");

    if (help_option) return;

    visual_config.set_depth_size (1);
    init_window (500, 500);

    gl.enable (GL.DEPTH_TEST);
    gl.enable (GL.LIGHT0);
    gl.enable (GL.LIGHTING);
  }


  protected void handle_button (int button, int state, int x, int y) {
    if (button == gnu.x11.Input.BUTTON1) {
      rotate_angle = (rotate_angle + 30) % 360;
      mark_window_dirty ();
    }
  }



  protected void handle_keyboard (int key, int state, int x, int y) {
    if (key == 'r' || key == 'R') {
      rotate_angle = (rotate_angle + 30) % 360;
      mark_window_dirty ();
    }
  }



  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);
    gl.push_matrix ();
    glu.look_at (0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);

    gl.push_matrix ();
    gl.rotated (rotate_angle, 1.0, 0.0, 0.0);
    gl.lightfv (GL.LIGHT0, GL.POSITION, LIGHT_POSITION);

    // light source
    gl.translated (0.0, 0.0, 1.5);
    gl.disable (GL.LIGHTING);
    gl.color3f (0.0f, 1.0f, 1.0f);
    glut.wire_cube (0.1f);
    gl.enable (GL.LIGHTING);
    gl.pop_matrix ();

    // torus
    glut.solid_torus (0.275, 0.85, 8, 15);
    gl.pop_matrix ();

    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();   
    double wh = (float) width / (float) height;
    glu.perspective (40.0, wh, 1.0, 20.0);
    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
  }


  public static void main (String [] args) {
    new LightPosition (args).exec ();
  }
}
