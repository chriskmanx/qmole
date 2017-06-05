package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Draw teapots with different material colors. A single light source
 * illuminates the objects. Modified from <code>teapot.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/TeapotMaterial.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/TeapotMaterial.help">
 * help output</a>
 */
public class TeapotMaterial extends gnu.x11.extension.glx.Application {
  private static final float [] LIGHT_AMBIENT = {0.0f, 0.0f, 0.0f, 1.0f};
  private static final float [] LIGHT_DIFFUSE = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float [] LIGHT_SPECULAR = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float [] LIGHT_POSITION = {0.0f, 3.0f, 3.0f, 0.0f};
  private static final float LIGHT_MODEL_LOCAL_VIEWER = 0.0f;

  private static final float [] LIGHT_MODEL_AMBIENT = {
    0.2f, 0.2f, 0.2f, 1.0f
  };


  private int rotate_x, rotate_y;
  private Teapot teapot;


  public TeapotMaterial (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "teapots of different material colors",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    visual_config.set_depth_size (1);
    init_window (500, 600);
    teapot = new Teapot (glut, 1.0);

    gl.enable (GL.AUTO_NORMAL);
    gl.enable (GL.DEPTH_TEST);
    gl.enable (GL.NORMALIZE);
    gl.front_face (GL.CW);
    
    init_light ();
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);

    /*  1st column: emerald, jade, obsidian, pearl, ruby, turquoise
     *  2nd column: brass, bronze, chrome, copper, gold, silver
     *  3rd column: black, cyan, green, red, white, yellow plastic
     *  4th column: black, cyan, green, red, white, yellow rubber
     */

    teapot.draw (2.0f, 17.0f, 0.0f, 0.0215f, 0.1745f, 0.0215f,
      0.07568f, 0.61424f, 0.07568f, 0.633f, 0.727811f, 0.633f, 0.6f);
    teapot.draw (2.0f, 14.0f, 0.0f, 0.135f, 0.2225f, 0.1575f,
      0.54f, 0.89f, 0.63f, 0.316228f, 0.316228f, 0.316228f, 0.1f);
    teapot.draw (2.0f, 11.0f, 0.0f, 0.05375f, 0.05f, 0.06625f,
      0.18275f, 0.17f, 0.22525f, 0.332741f, 0.328634f, 0.346435f, 0.3f);
    teapot.draw (2.0f, 8.0f, 0.0f, 0.25f, 0.20725f, 0.20725f,
      1f, 0.829f, 0.829f, 0.296648f, 0.296648f, 0.296648f, 0.088f);
    teapot.draw (2.0f, 5.0f, 0.0f, 0.1745f, 0.01175f, 0.01175f,
      0.61424f, 0.04136f, 0.04136f, 0.727811f, 0.626959f, 0.626959f, 0.6f);
    teapot.draw (2.0f, 2.0f, 0.0f, 0.1f, 0.18725f, 0.1745f,
      0.396f, 0.74151f, 0.69102f, 0.297254f, 0.30829f, 0.306678f, 0.1f);
    teapot.draw (6.0f, 17.0f, 0.0f, 0.329412f, 0.223529f, 0.027451f,
      0.780392f, 0.568627f, 0.113725f, 0.992157f, 0.941176f, 0.807843f,
      0.21794872f);
    teapot.draw (6.0f, 14.0f, 0.0f, 0.2125f, 0.1275f, 0.054f,
      0.714f, 0.4284f, 0.18144f, 0.393548f, 0.271906f, 0.166721f, 0.2f);
    teapot.draw (6.0f, 11.0f, 0.0f, 0.25f, 0.25f, 0.25f,
      0.4f, 0.4f, 0.4f, 0.774597f, 0.774597f, 0.774597f, 0.6f);
    teapot.draw (6.0f, 8.0f, 0.0f, 0.19125f, 0.0735f, 0.0225f,
      0.7038f, 0.27048f, 0.0828f, 0.256777f, 0.137622f, 0.086014f, 0.1f);
    teapot.draw (6.0f, 5.0f, 0.0f, 0.24725f, 0.1995f, 0.0745f,
      0.75164f, 0.60648f, 0.22648f, 0.628281f, 0.555802f, 0.366065f, 0.4f);
    teapot.draw (6.0f, 2.0f, 0.0f, 0.19225f, 0.19225f, 0.19225f,
      0.50754f, 0.50754f, 0.50754f, 0.508273f, 0.508273f, 0.508273f, 0.4f);
    teapot.draw (10.0f, 17.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.01f, 0.01f, 0.01f,
      0.50f, 0.50f, 0.50f, 0.25f);
    teapot.draw (10.0f, 14.0f, 0.0f, 0.0f, 0.1f, 0.06f, 0.0f, 0.50980392f, 
      0.50980392f, 0.50196078f, 0.50196078f, 0.50196078f, 0.25f);
    teapot.draw (10.0f, 11.0f, 0.0f, 0.0f, 0.0f, 0.0f,
      0.1f, 0.35f, 0.1f, 0.45f, 0.55f, 0.45f, 0.25f);
    teapot.draw (10.0f, 8.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.5f, 0.0f, 0.0f,
      0.7f, 0.6f, 0.6f, 0.25f);
    teapot.draw (10.0f, 5.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.55f, 0.55f, 0.55f,
      0.70f, 0.70f, 0.70f, 0.25f);
    teapot.draw (10.0f, 2.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.5f, 0.5f, 0.0f,
      0.60f, 0.60f, 0.50f, 0.25f);
    teapot.draw (14.0f, 17.0f, 0.0f, 0.02f, 0.02f, 0.02f, 0.01f, 0.01f, 0.01f,
      0.4f, 0.4f, 0.4f, 0.078125f);
    teapot.draw (14.0f, 14.0f, 0.0f, 0.0f, 0.05f, 0.05f, 0.4f, 0.5f, 0.5f,
      0.04f, 0.7f, 0.7f, 0.078125f);
    teapot.draw (14.0f, 11.0f, 0.0f, 0.0f, 0.05f, 0.0f, 0.4f, 0.5f, 0.4f,
      0.04f, 0.7f, 0.04f, 0.078125f);
    teapot.draw (14.0f, 8.0f, 0.0f, 0.05f, 0.0f, 0.0f, 0.5f, 0.4f, 0.4f,
      0.7f, 0.04f, 0.04f, 0.078125f);
    teapot.draw (14.0f, 5.0f, 0.0f, 0.05f, 0.05f, 0.05f, 0.5f, 0.5f, 0.5f,
      0.7f, 0.7f, 0.7f, 0.078125f);
    teapot.draw (14.0f, 2.0f, 0.0f, 0.05f, 0.05f, 0.0f, 0.5f, 0.5f, 0.4f,
      0.7f, 0.7f, 0.04f, 0.078125f);

    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();

    double wh = (float) width / (float) height;
    double hw = (float) height / (float) width;
    
    if (width <= height)
      gl.ortho (0.0, 16.0, 0.0, 16.0*hw, -10.0, 10.0);
    else
      gl.ortho (0.0, 16.0*wh, 0.0, 16.0, -10.0, 10.0);

    gl.matrix_mode (GL.MODELVIEW);
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


  public static void main (String [] args) {
    new TeapotMaterial (args).exec ();
  }
}
