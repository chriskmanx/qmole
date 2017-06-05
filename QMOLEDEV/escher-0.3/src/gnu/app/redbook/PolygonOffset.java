package gnu.app.redbook;

import gnu.x11.extension.glx.GL;
import gnu.x11.Input;


/**
 * Test polygon offset. To demonstrate polygon offset to draw a shaded
 * polygon and its wireframe counterpart without ugly visual artifacts
 * ("stitching"). Modified from <code>polyoff.c</code>.
 *
 * <ul>
 * <li>To increase t-distance, press 't'.
 * <li>To decrease t-distance, press 'T'.
 * <li>To decrease offset factor, press 'f'.
 * <li>To increase offset factor, press 'F'.
 * <li>To decrease offset units, press 'u'.
 * <li>To increase offset units, press 'U'.
 * <li>To rotate about x-axis, press 'x', 'X', BUTTON1.
 * <li>To rotate about y-axis, press 'y', 'Y', BUTTON2.
 * </ul>
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/PolygonOffset.gif">
 * screenshot 8</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/PolygonOffset.help">
 * help output</a>
 */
public class PolygonOffset extends gnu.x11.extension.glx.Application {
  private static final float [] LIGHT_AMBIENT = {0.0f, 0.0f, 0.0f, 1.0f};
  private static final float [] LIGHT_DIFFUSE = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float [] LIGHT_SPECULAR = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float [] LIGHT_POSITION = {1.0f, 1.0f, 1.0f, 0.0f};

  private static final float [] MATERIAL_AMBIENT
    = {0.8f, 0.8f, 0.8f, 1.0f};
  private static final float [] MATERIAL_SPECULAR
    = {0.0f, 0.0f, 0.0f, 1.0f};
  private static final float MATERIAL_SHININESS = 0.0f;

  private static final float [] LIGHT_MODEL_AMBIENT
    = {0.2f, 0.2f, 0.2f, 1.0f};


  private int display_list;
  private float offset_factor = 1.0f;
  private float offset_units = 1.0f;
  private float t_distance;
  private int x_angle, y_angle;


  public PolygonOffset (String [] args) {
    super (args, BUTTON_PRESS_BIT | KEYBOARD_BIT | RESIZE_BIT);

    about ("0.1", "polygon offset",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo increase t-distance, press 't'."
      + "\nTo decrease t-distance, press 'T'."
      + "\nTo decrease offset factor, press 'f'."
      + "\nTo increase offset factor, press 'F'."
      + "\nTo decrease offset units, press 'u'."
      + "\nTo increase offset units, press 'U'."
      + "\nTo rotate about x-axis, press 'x', 'X', BUTTON1."
      + "\nTo rotate about y-axis, press 'y', 'Y', BUTTON2.");

    if (help_option) return;

    visual_config.set_depth_size (1);
    init_window (300, 300);

    gl.enable (GL.DEPTH_TEST);
    init_light ();
    init_sphere ();
  }

   
  protected void handle_button (int button, int state, int x, int y) {
    switch (button) {
    case Input.BUTTON1: x_angle = (x_angle + 5) % 360; break;
    case Input.BUTTON2: y_angle = (y_angle + 5) % 360; break;
    default: return;
    }

    mark_window_dirty ();
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);
    gl.push_matrix ();
    gl.translatef (0.0f, 0.0f, t_distance);
    gl.rotatef (x_angle, 1.0f, 0.0f, 0.0f);
    gl.rotatef (y_angle, 0.0f, 1.0f, 0.0f);

    gl.enable (GL.LIGHTING);
    gl.enable (GL.LIGHT0);
    gl.enable (GL.POLYGON_OFFSET_FILL);
    gl.polygon_offset (offset_factor, offset_units);
    gl.call_list (display_list);
    gl.disable (GL.POLYGON_OFFSET_FILL);

    gl.disable (GL.LIGHTING);
    gl.disable (GL.LIGHT0);
    gl.color3f (1.0f, 1.0f, 1.0f);
    gl.polygon_mode (GL.FRONT_AND_BACK, GL.LINE);
    gl.call_list (display_list);
    gl.polygon_mode (GL.FRONT_AND_BACK, GL.FILL);

    gl.pop_matrix ();
    gl.swap_buffers (window);
  }


  protected void handle_keyboard (int key, int state, int x, int y) {
    switch (key) {
    case 't': 
      if (t_distance >= 4.0f) return;
      t_distance += 0.5f;
      break;

    case 'T':
      if (t_distance <= -5.0f) return;
      t_distance -= 0.5f;
      break;

    case 'f':
      offset_factor -= 0.1f;
      System.out.println ("offset-factor: " + offset_factor);
      break;

    case 'F':
      offset_factor += 0.1f;
      System.out.println ("offset-factor: " + offset_factor);
      break;

    case 'u':
      offset_units -= 0.1f;
      System.out.println ("offset-units: " + offset_units);
      break;

    case 'U':
      offset_units += 0.1f;
      System.out.println ("offset-units: " + offset_units);
      break;

    case 'x':                   // fall through
    case 'X': x_angle = (x_angle + 5) % 360; break;

    case 'y':                   // fall through
    case 'Y': y_angle = (y_angle + 5) % 360; break;

    default: return;
    }

    mark_window_dirty ();
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();
    double wh = (float) width / (float) height;
    glu.perspective (45.0, wh, 1.0, 10.0);
    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();   
    glu.look_at (0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
  }


  private void init_sphere () {
    display_list = gl.gen_lists (1);
    gl.new_list (display_list, GL.COMPILE);
    glut.solid_sphere (1.0, 20, 12);
    gl.end_list ();
  }


  private void init_light () {
    gl.lightfv (GL.LIGHT0, GL.AMBIENT, LIGHT_AMBIENT);
    gl.lightfv (GL.LIGHT0, GL.DIFFUSE, LIGHT_DIFFUSE);
    gl.lightfv (GL.LIGHT0, GL.POSITION, LIGHT_POSITION);
    gl.lightfv (GL.LIGHT0, GL.SPECULAR, LIGHT_SPECULAR);
    gl.light_modelfv (GL.LIGHT_MODEL_AMBIENT, LIGHT_MODEL_AMBIENT);

    gl.materialfv (GL.FRONT, GL.AMBIENT_AND_DIFFUSE, MATERIAL_AMBIENT);
    gl.materialfv (GL.FRONT, GL.SPECULAR, MATERIAL_SPECULAR);
    gl.materialf (GL.FRONT, GL.SHININESS, MATERIAL_SHININESS);
  }


  public static void main (String [] args) {
    new PolygonOffset (args).exec ();
  }
}
