package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Anti-alias with accumulation buffer and orthographic parallel
 * projection. Modified from <code>accanti.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/AntiAliasParallel.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/AntiAliasParallel.help">
 * help output</a>
 */
public class AntiAliasParallel extends gnu.x11.extension.glx.Application {
  private static final int ACCUM_SIZE = 8;

  private static final float [] TORUS_DIFFUSE
    = {0.7f, 0.7f, 0.0f, 1.0f};
  private static final float [] CUBE_DIFFUSE
    = {0.0f, 0.7f, 0.7f, 1.0f};
  private static final float [] SPHERE_DIFFUSE
    = {0.7f, 0.0f, 0.7f, 1.0f};
  private static final float [] OCTAHEDRON_DIFFUSE
    = {0.7f, 0.4f, 0.4f, 1.0f};

  private static final float [] LIGHT_MODEL_AMBIENT
    = {0.2f, 0.2f, 0.2f, 1.0f};
  private static final float [] LIGHT_POSITION
    = {0.0f, 0.0f, 10.0f, 1.0f};
  private static final float [] MATERIAL_AMBIENT
    = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float [] MATERIAL_SPECULAR 
    = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float MATERIAL_SHININESS = 50.0f;


  public AntiAliasParallel (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "anti-alias with parallel projection",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    visual_config.set_accum_rgb_size (ACCUM_SIZE);
    visual_config.set_depth_size (1);
    init_window (250, 250);

    gl.enable (GL.DEPTH_TEST);
    gl.shade_model (GL.FLAT);
    init_light ();
  }


  private void draw_scene () {
    gl.push_matrix ();
    gl.rotatef (30.0f, 1.0f, 0.0f, 0.0f);

    // torus
    gl.push_matrix ();
    gl.translatef (-0.80f, 0.35f, 0.0f); 
    gl.rotatef (100.0f, 1.0f, 0.0f, 0.0f);
    gl.materialfv (GL.FRONT, GL.DIFFUSE, TORUS_DIFFUSE);
    glut.solid_torus (0.275, 0.85, 16, 16);
    gl.pop_matrix ();

    // cube
    gl.push_matrix ();
    gl.translatef (-0.75f, -0.50f, 0.0f); 
    gl.rotatef (45.0f, 0.0f, 0.0f, 1.0f);
    gl.rotatef (45.0f, 1.0f, 0.0f, 0.0f);
    gl.materialfv (GL.FRONT, GL.DIFFUSE, CUBE_DIFFUSE);
    glut.solid_cube (1.5f);
    gl.pop_matrix ();

    // sphere
    gl.push_matrix ();
    gl.translatef (0.75f, 0.60f, 0.0f); 
    gl.rotatef (30.0f, 1.0f, 0.0f, 0.0f);
    gl.materialfv (GL.FRONT, GL.DIFFUSE, SPHERE_DIFFUSE);
    glut.solid_sphere (1.0, 16, 16);
    gl.pop_matrix ();

    // octahedron
    gl.push_matrix ();
    gl.translatef (0.70f, -0.90f, 0.25f); 
    gl.materialfv (GL.FRONT, GL.DIFFUSE, OCTAHEDRON_DIFFUSE);
    glut.solid_octahedron ();
    gl.pop_matrix ();
    
    gl.pop_matrix ();
  }
    
  
  protected void handle_expose () {
    gl.clear (GL.ACCUM_BUFFER_BIT);

    for (int i=0; i<ACCUM_SIZE; i++) {
      gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);
      gl.push_matrix ();
      jitter_position (i);
      draw_scene ();
      gl.pop_matrix ();
      gl.accum (GL.ACCUM, 1.0f/ACCUM_SIZE);
    }
      
    gl.accum (GL.RETURN, 1.0f);
    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();

    double wh = (float) width / (float) height;
    double hw = (float) height / (float) width;
    
    if (width <= height)
      gl.ortho (-2.25, 2.25, -2.25*hw, 2.25*hw, -10.0, 10.0);
    else
      gl.ortho (-2.25*wh, 2.25*wh, -2.25, 2.25, -10.0, 10.0);

    gl.matrix_mode (GL.MODELVIEW);
  }


  private void init_light () {
    gl.enable (GL.LIGHTING);
    gl.enable (GL.LIGHT0);

    gl.lightfv (GL.LIGHT0, GL.POSITION, LIGHT_POSITION);
    gl.light_modelfv (GL.LIGHT_MODEL_AMBIENT, LIGHT_MODEL_AMBIENT);

    gl.materialfv (GL.FRONT, GL.AMBIENT, MATERIAL_AMBIENT);
    gl.materialfv (GL.FRONT, GL.SPECULAR, MATERIAL_SPECULAR);
    gl.materialf (GL.FRONT, GL.SHININESS, MATERIAL_SHININESS);
  }


  private void jitter_position (int i) {
    float [] [] jitter = Jitter.J8;

    /* Note that 4.5 is the distance in world space between left and right
     * and bottom and top.
     *
     * This formula converts fractional pixel movement to world
     * coordinates.
     */
    float x = jitter [i] [0] * 4.5f / window.width;
    float y = jitter [i] [1] * 4.5f / window.height;

    gl.translatef (x, y, 0.0f);
  }


  public static void main (String [] args) {
    new AntiAliasParallel (args).exec ();
  }
}
