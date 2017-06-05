package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Test fog. It draws 5 red spheres, each at a different z distance from
 * the eye, in different types of fog. In this program, there is a fixed
 * density value, as well as fixed start and end values for the linear fog. 
 * Modified from <code>fog.c</code>.
 *
 * <ul>
 * <li>To use exponential fog, press 'e' or 'E'.
 * <li>To use exponential squared fog, press 's' or 'S'.
 * <li>To use linear fog, press 'l' or 'L'.
 * </ul>
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Fog.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Fog.help">
 * help output</a>
 */
public class Fog extends gnu.x11.extension.glx.Application {
  private static final float [] FOG_COLOR = {0.5f, 0.5f, 0.5f, 1.0f};
  private static final float [] LIGHT_MODEL_AMBIENT
    = {0.2f, 0.2f, 0.2f, 1.0f};
  private static final float LIGHT_MODEL_LOCAL_VIEWER = 0.0f;
  private static final float [] LIGHT_POSITION = {0.5f, 0.5f, 3.0f, 0.0f};

  private static final float [] MATERIAL_AMBIENT
    = {0.61424f, 0.04136f, 0.04136f, 0.0f};
  private static final float [] MATERIAL_DIFFUSE
    = {0.61424f, 0.04136f, 0.04136f, 0.0f};
  private static final float MATERIAL_SHININESS = 0.6f * 128.0f;
  private static final float [] MATERIAL_SPECULAR
    = {0.727811f, 0.626959f, 0.626959f, 0.0f};


  public Fog (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "fogged spheres",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "To use exponential fog, press 'e' or 'E'."
      + "To use exponential squared fog, press 's' or 'S'."
      + "To use linear fog, press 'l' or 'L'.");

    if (help_option) return;

    visual_config.set_depth_size (1);
    init_window (500, 500);

    gl.clear_color (0.5f, 0.5f, 0.5f, 1.0f);
    gl.enable (GL.AUTO_NORMAL);
    gl.enable (GL.DEPTH_TEST);
    gl.enable (GL.FOG);
    gl.enable (GL.NORMALIZE);
    gl.front_face (GL.CW);
    init_fog ();
    init_light ();
  }


  private void draw_sphere (float x, float y, float z) {
   gl.push_matrix ();
   gl.translatef (x, y, z);
   glut.solid_sphere (0.4, 16, 16);
   gl.pop_matrix ();
  }

  
  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);
    draw_sphere (-2.0f, -0.5f, -1.0f);
    draw_sphere (-1.0f, -0.5f, -2.0f);
    draw_sphere (0.0f, -0.5f, -3.0f);
    draw_sphere (1.0f, -0.5f, -4.0f);
    draw_sphere (2.0f, -0.5f, -5.0f);
    gl.swap_buffers (window);
  }


  protected void handle_keyboard (int key, int state, int x, int y) {
    switch (key) {
    case 'e':                   // fall through
    case 'E': gl.fogi (GL.FOG_MODE, GL.EXP); break;
    case 's':                   // fall through
    case 'S': gl.fogi (GL.FOG_MODE, GL.EXP2); break;

    case 'l':                   // fall through
    case 'L':                   
      gl.fogf (GL.FOG_START, 1.0f);
      gl.fogf (GL.FOG_END, 5.0f);
      gl.fogi (GL.FOG_MODE, GL.LINEAR);
      break;

    default: return;
    }

    mark_window_dirty ();
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();

    double wh = (float) width / (float) height;
    double hw = (float) height / (float) width;
    
    if (width <= height)
      gl.ortho (-2.5, 2.5, -2.5*hw, 2.5*hw, -10.0, 10.0);
    else
      gl.ortho (-2.5*wh, 2.5*wh, -2.5, 2.5, -10.0, 10.0);

    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
  }


  private void init_fog () {
    gl.enable (GL.FOG);
    gl.fogi (GL.FOG_MODE, GL.EXP);
    gl.fogfv (GL.FOG_COLOR, FOG_COLOR);
    gl.fogf (GL.FOG_DENSITY, 0.35f);
    gl.hint (GL.FOG_HINT, GL.DONT_CARE);
  }


  private void init_light () {
    gl.enable (GL.LIGHTING);
    gl.enable (GL.LIGHT0);

    gl.lightfv (GL.LIGHT0, GL.POSITION, LIGHT_POSITION);
    gl.light_modelf (GL.LIGHT_MODEL_LOCAL_VIEWER,
      LIGHT_MODEL_LOCAL_VIEWER);

    gl.materialfv (GL.FRONT, GL.DIFFUSE, MATERIAL_AMBIENT);
    gl.materialfv (GL.FRONT, GL.DIFFUSE, MATERIAL_DIFFUSE);
    gl.materialfv (GL.FRONT, GL.SPECULAR, MATERIAL_SPECULAR);
    gl.materialf (GL.FRONT, GL.SHININESS, MATERIAL_SHININESS);
  }


  public static void main (String [] args) {
    new Fog (args).exec ();
  }
}
