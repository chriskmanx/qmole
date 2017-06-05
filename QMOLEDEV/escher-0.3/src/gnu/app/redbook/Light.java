package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Test lighting model. A sphere is drawn using a grey material
 * characteristic. A single light source illuminates the object. Modified
 * from <code>light.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Light.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Light.help">
 * help output</a>
 */
public class Light extends gnu.x11.extension.glx.Application {
  private static final float [] LIGHT_POSITION
    = {1.0f, 1.0f, 1.0f, 0.0f};
  private static final float [] MATERIAL_SPECULAR
    = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float MATERIAL_SHININESS = 25.0f;


  private static final float [] material_diffuse 
    = {0.5f, 0.5f, 0.5f, 1.0f};


  public Light (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "lighting model",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    visual_config.set_depth_size (1);
    init_window (500, 500);

    gl.enable (GL.DEPTH_TEST);
    init_light ();
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
    gl.materialfv (GL.FRONT, GL.SPECULAR, MATERIAL_SPECULAR);
    gl.materialf (GL.FRONT, GL.SHININESS, MATERIAL_SHININESS);

    // CAUTION call after `gl.color_material'
    gl.enable (GL.COLOR_MATERIAL);
  }


  public static void main (String [] args) {
    new Light (args).exec ();
  }
}
