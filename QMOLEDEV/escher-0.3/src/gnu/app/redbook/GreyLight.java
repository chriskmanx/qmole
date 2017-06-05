package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Test lighting model with grey. Quadric objects are drawn using a grey
 * material characteristic. A single light source illuminates the objects. 
 * Modified from <code>scene.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/GreyLight.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/GreyLight.help">
 * help output</a>
 */
public class GreyLight extends gnu.x11.extension.glx.Application {
  private static final float [] LIGHT_AMBIENT = {0.0f, 0.0f, 0.0f, 1.0f};
  private static final float [] LIGHT_DIFFUSE = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float [] LIGHT_SPECULAR = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float [] LIGHT_POSITION = {1.0f, 1.0f, 1.0f, 0.0f};


  public GreyLight (String [] args) {
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
    gl.push_matrix ();
    gl.rotatef (20.0f, 1.0f, 0.0f, 0.0f);

    // torus
    gl.push_matrix ();
    gl.translatef (-0.75f, 0.5f, 0.0f); 
    gl.rotatef (90.0f, 1.0f, 0.0f, 0.0f);
    glut.solid_torus (0.275f, 0.85f, 15, 15);
    gl.pop_matrix ();

    // cone
    gl.push_matrix ();
    gl.translatef (-0.75f, -0.5f, 0.0f); 
    gl.rotatef (270.0f, 1.0f, 0.0f, 0.0f);
    glut.solid_cone (1.0f, 2.0f, 15, 15);
    gl.pop_matrix ();

    // sphere
    gl.push_matrix ();
    gl.translatef (0.75f, 0.0f, -1.0f); 
    glut.solid_sphere (1.0f, 15, 15);
    gl.pop_matrix ();
   
    gl.pop_matrix ();
    gl.swap_buffers (window);
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


  private void init_light () {
    gl.enable (GL.LIGHTING);
    gl.enable (GL.LIGHT0);

    gl.lightfv (GL.LIGHT0, GL.AMBIENT, LIGHT_AMBIENT);
    gl.lightfv (GL.LIGHT0, GL.DIFFUSE, LIGHT_DIFFUSE);
    gl.lightfv (GL.LIGHT0, GL.POSITION, LIGHT_POSITION);
    gl.lightfv (GL.LIGHT0, GL.SPECULAR, LIGHT_SPECULAR);
  }


  public static void main (String [] args) {
    new GreyLight (args).exec ();
  }
}
