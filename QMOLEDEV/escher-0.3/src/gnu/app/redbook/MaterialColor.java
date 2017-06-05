package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Draw a sphere with material color. Modified from <code>colormat.c</code>.
 *
 * <ul>
 * <li>To change red value, press 'r', 'R' or BUTTON1.
 * <li>To change green value, press 'g', 'G' or BUTTON2.
 * <li>To change blue value, press 'b', 'B', or BUTTON3.
 * </ul>
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/MaterialColor.gif">
 * screenshot 8</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/MaterialColor.help">
 * help output</a>
 */
public class MaterialColor extends gnu.x11.extension.glx.Application {
  private static final float [] LIGHT_POSITION
    = {1.0f, 1.0f, 1.0f, 0.0f};
  private static final float [] MATERIAL_SPECULAR
    = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float MATERIAL_SHININESS = 25.0f;


  private static final float [] material_diffuse 
    = {0.5f, 0.5f, 0.5f, 1.0f};


  public MaterialColor (String [] args) {
    super (args, BUTTON_PRESS_BIT | KEYBOARD_BIT | RESIZE_BIT);

    about ("0.1", "material color",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo change red value, press 'r', 'R' or BUTTON1."
      + "\nTo change green value, press 'g', 'G' or BUTTON2."
      + "\nTo change blue value, press 'b', 'B', or BUTTON3.");

    if (help_option) return;

    visual_config.set_depth_size (1);
    init_window (500, 500);

    gl.enable (GL.DEPTH_TEST);
    init_light ();
  }


  private void change_diffuse (int i) {
    material_diffuse [i] += 0.1f;
    if (material_diffuse [i] > 1.0f) material_diffuse [i] = 0.0f;
    gl.color4fv (material_diffuse);
    mark_window_dirty ();
  }


  protected void handle_button (int button, int state, int x, int y) {
    int i = button - gnu.x11.Input.BUTTON1;
    if (i >= 3) return;
    change_diffuse (i);
  }



  protected void handle_keyboard (int key, int state, int x, int y) {
    switch (key) {
    case 'r':                   // fall through
    case 'R': change_diffuse (0); break;
    case 'g':                   // fall through
    case 'G': change_diffuse (1);  break;
    case 'b':                   // fall through
    case 'B': change_diffuse (2); break;
    }
  }



  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);
    glut.solid_sphere (1.0, 20, 16);
    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();

    double wh = (float) width / (float) height;
    double hw = (float) height / (float) width;
    
    if (width <= height)
      gl.ortho (-1.5, 1.5, -1.5*hw, 1.5*hw, -10.0, 10.0);
    else
      gl.ortho (-1.5*wh, 1.5*wh, -1.5, 1.5, -10.0, 10.0);

    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
  }


  private void init_light () {
    gl.enable (GL.LIGHT0);
    gl.enable (GL.LIGHTING);

    gl.color_material (GL.FRONT, GL.DIFFUSE);
    gl.lightfv (GL.LIGHT0, GL.POSITION, LIGHT_POSITION);
    gl.materialfv (GL.FRONT, GL.DIFFUSE, material_diffuse);
    gl.materialfv (GL.FRONT, GL.SPECULAR, MATERIAL_SPECULAR);
    gl.materialf (GL.FRONT, GL.SHININESS, MATERIAL_SHININESS);

    // CAUTION call after `gl.color_material'
    gl.enable (GL.COLOR_MATERIAL);
  }


  public static void main (String [] args) {
    new MaterialColor (args).exec ();
  }
}
