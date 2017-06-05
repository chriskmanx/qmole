package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Create out-of-focus depth-of-field effect. To demonstrate the use of the
 * accumulation buffer and perspective projection. The teapots are drawn
 * several times into the accumulation buffer. The viewing volume is
 * jittered, except at the focal point, where the viewing volume is at the
 * same position, each time. In this case, the gold teapot remains in
 * focus. Modified from <code>dof.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/DepthOfField.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/DepthOfField.help">
 * help output</a>
 */
public class DepthOfField extends gnu.x11.extension.glx.Application {
  private static final int ACCUM_SIZE = 8;

  private static final float [] LIGHT_AMBIENT = {0.0f, 0.0f, 0.0f, 1.0f};
  private static final float [] LIGHT_DIFFUSE = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float [] LIGHT_SPECULAR = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float [] LIGHT_POSITION = {0.0f, 3.0f, 3.0f, 0.0f};
  private static final float LIGHT_MODEL_LOCAL_VIEWER = 0.0f;

  private static final float [] LIGHT_MODEL_AMBIENT = {
    0.2f, 0.2f, 0.2f, 1.0f
  };


  private Teapot teapot;


  public DepthOfField (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "depth-of-field effect",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    visual_config.set_accum_rgb_size (ACCUM_SIZE);
    visual_config.set_depth_size (1);
    init_window (300, 300);
    teapot = new Teapot (glut, 0.5);

    gl.enable (GL.AUTO_NORMAL);
    gl.enable (GL.DEPTH_TEST);
    gl.enable (GL.NORMALIZE);
    gl.front_face (GL.CW);

    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
    init_light ();
  }

  
  private void draw_scene () {
    teapot.draw (-1.1f, -0.5f, -4.5f, 0.1745f, 0.01175f, // ruby
      0.01175f, 0.61424f, 0.04136f, 0.04136f, 
      0.727811f, 0.626959f, 0.626959f, 0.6f);
    teapot.draw (-0.5f, -0.5f, -5.0f, 0.24725f, 0.1995f, // gold
      0.0745f, 0.75164f, 0.60648f, 0.22648f, 
      0.628281f, 0.555802f, 0.366065f, 0.4f);
    teapot.draw (0.2f, -0.5f, -5.5f, 0.19225f, 0.19225f, // silver
      0.19225f, 0.50754f, 0.50754f, 0.50754f, 
      0.508273f, 0.508273f, 0.508273f, 0.4f);
    teapot.draw (1.0f, -0.5f, -6.0f, 0.0215f, 0.1745f, 0.0215f, // emerald
      0.07568f, 0.61424f, 0.07568f, 0.633f, 
      0.727811f, 0.633f, 0.6f);
    teapot.draw (1.8f, -0.5f, -6.5f, 0.0f, 0.1f, 0.06f, 0.0f, // cyan
      0.50980392f, 0.50980392f, 0.50196078f, 
      0.50196078f, 0.50196078f, 0.25f);
  }
    
  
  protected void handle_expose () {
    /* It draws five teapots into the accumulation buffer several times;
     * each time with a jittered perspective.
     */

    gl.clear (GL.ACCUM_BUFFER_BIT);

    for (int i=0; i<ACCUM_SIZE; i++) {
      gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);
      jitter_position (i);
      draw_scene ();
      gl.accum (GL.ACCUM, 1.0f/ACCUM_SIZE);
    }
      
    gl.accum (GL.RETURN, 1.0f);
    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
  }


  private void init_light () {
    gl.enable (GL.LIGHTING);
    gl.enable (GL.LIGHT0);

    gl.lightfv (GL.LIGHT0, GL.AMBIENT, LIGHT_AMBIENT);
    gl.lightfv (GL.LIGHT0, GL.DIFFUSE, LIGHT_DIFFUSE);
    gl.lightfv (GL.LIGHT0, GL.POSITION, LIGHT_POSITION);
    gl.lightfv (GL.LIGHT0, GL.SPECULAR, LIGHT_SPECULAR);

    gl.light_modelfv (GL.LIGHT_MODEL_AMBIENT, LIGHT_MODEL_AMBIENT);
    gl.light_modelf (GL.LIGHT_MODEL_LOCAL_VIEWER, 
      LIGHT_MODEL_LOCAL_VIEWER);
  }


  private void jitter_position (int i) {
    /* The focal point is at z = 5.0, so the gold teapot will stay in
     * focus. The amount of jitter is adjusted by the magnitude of the
     * `AntiAlias.perspective' jitter; in this example, 0.33. In this
     * example, the teapots are drawn eight times. See `Jitter'.
     */

    float [] [] jitter = Jitter.J8;
    float x = 0.33f * jitter [i] [0];
    float y = 0.33f * jitter [i] [1];
    double wh = (double) window.width / (double) window.height;

    AntiAlias.perspective (gl, window.width, window.height,
      45.0, wh, 1.0, 15.0, 0.0, 0.0, x, y, 5.0);
  }


  public static void main (String [] args) {
    new DepthOfField (args).exec ();
  }
}
