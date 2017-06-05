package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Draw a robot's arm. To demonstrate how to composite modeling
 * transformations to draw translated and hierarchical models. To rotate
 * robot's shoulder, press 's' or 'S'. To rotate robot's elbow, press 'e'
 * or 'E'. Modified from <code>robot.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Robot.gif">
 * screenshot 6</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Robot.help">
 * help output</a>
 */
public class Robot extends gnu.x11.extension.glx.Application {
  private int shoulder_angle, elbow_angle;


  public Robot (String [] args) {
    super (args, KEYBOARD_BIT | RESIZE_BIT);

    about ("0.1", "robot's arm",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo rotate robot's shoulder, press 's' or 'S'."
      + "\nTo rotate robot's elbow, press 'e' or 'E'.");

    if (help_option) return;

    visual_config.set_double_buffer ();
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
    gl.push_matrix ();

    // shoulder
    gl.translatef (-1.0f, 0.0f, 0.0f);
    gl.rotatef (shoulder_angle, 0.0f, 0.0f, 1.0f);
    gl.translatef (1.0f, 0.0f, 0.0f);
    gl.push_matrix ();
    gl.scalef (2.0f, 0.4f, 1.0f);
    glut.wire_cube (1.0f);
    gl.pop_matrix ();

    // elbow
    gl.translatef (1.0f, 0.0f, 0.0f);
    gl.rotatef (elbow_angle, 0.0f, 0.0f, 1.0f);
    gl.translatef (1.0f, 0.0f, 0.0f);
    gl.push_matrix ();
    gl.scalef (2.0f, 0.4f, 1.0f);
    glut.wire_cube (1.0f);
    gl.pop_matrix ();

    gl.pop_matrix ();
    gl.swap_buffers (window);
  }


  protected void handle_keyboard (int key, int state, int x, int y) {
    switch (key) {
    case 'e': elbow_angle = (elbow_angle + 5) % 360; break;
    case 'E': elbow_angle = (elbow_angle - 5) % 360; break;
    case 's': shoulder_angle = (shoulder_angle + 5) % 360; break;
    case 'S': shoulder_angle = (shoulder_angle - 5) % 360; break;
    default: return;
    }

    mark_window_dirty ();
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();    
    double wh = (float) width / (float) height;
    glu.perspective (65.0, wh, 1.0, 20.0);
    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
    gl.translatef (0.0f, 0.0f, -5.0f);
  }


  public static void main (String [] args) {
    new Robot (args).exec ();
  }
}
