package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Test lighting model with color. To demonstrate the use of the GL
 * lighting model. Several objects are drawn using different material
 * characteristics. A single light source illuminates the objects. Modified
 * from <code>material.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/ColorLight.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/ColorLight.help">
 * help output</a>
 */
public class ColorLight extends gnu.x11.extension.glx.Application {
  private static final float [] LIGHT_AMBIENT = {0.0f, 0.0f, 0.0f, 1.0f};
  private static final float [] LIGHT_DIFFUSE = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float [] LIGHT_SPECULAR = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float [] LIGHT_POSITION = {0.0f, 3.0f, 2.0f, 0.0f};
  private static final float LIGHT_MODEL_LOCAL_VIEWER = 0.0f;
  private static final float MATERIAL_SHININESS_NO = 0.0f;
  private static final float MATERIAL_SHININESS_LOW = 5.0f;
  private static final float MATERIAL_SHININESS_HIGH = 100.0f;

  private static final float [] LIGHT_MODEL_AMBIENT
    = {0.4f, 0.4f, 0.4f, 1.0f};
  private static final float [] MATERIAL_AMBIENT_COLOR
    = {0.8f, 0.8f, 0.2f, 1.0f};
  private static final float [] MATERIAL_AMBIENT_GREY
    = {0.7f, 0.7f, 0.7f, 1.0f};
  private static final float [] MATERIAL_DIFFUSE
    = {0.1f, 0.5f, 0.8f, 1.0f};
  private static final float [] MATERIAL_EMISSION
    = {0.3f, 0.2f, 0.2f, 0.0f};
  private static final float [] MATERIAL_NONE
    = {0.0f, 0.0f, 0.0f, 1.0f};
  private static final float [] MATERIAL_SPECULAR
    = {1.0f, 1.0f, 1.0f, 1.0f};


  public ColorLight (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "lighting model with color",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    visual_config.set_depth_size (1);
    init_window (600, 450);

    gl.enable (GL.DEPTH_TEST);
    gl.clear_color (0.0f, 0.1f, 0.1f, 0.0f);
    init_light ();
  }


  private void draw_sphere (float x, float y, float z, 
    float [] ambient, float [] diffuse, float [] specular, float shininess,
    float [] emission) {

    gl.push_matrix ();
    gl.translatef  (x, y, z);
    gl.materialfv (GL.FRONT, GL.AMBIENT, ambient);
    gl.materialfv (GL.FRONT, GL.DIFFUSE, diffuse);
    gl.materialfv (GL.FRONT, GL.SPECULAR, specular);
    gl.materialf (GL.FRONT, GL.SHININESS, shininess);
    gl.materialfv (GL.FRONT, GL.EMISSION, emission);
    glut.solid_sphere (1.0, 16, 16);
    gl.pop_matrix ();       
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);

    /* Twelve spheres in 3 rows and 4 columns.
     * 
     * first row: no ambient reflection
     * second row: significant ambient reflection
     * third row: colored ambient reflection
     *
     * first column: blue diffuse reflection only
     * second column: blue diffuse reflection
     *   + specular reflection with low shininess exponent
     * third column: blue diffuse reflection
     *   + specular reflection with high shininess exponent
     * fourth column: with emissive component 
     */

    draw_sphere (-3.75f, 3.0f, 0.0f, 
      MATERIAL_NONE, MATERIAL_DIFFUSE, MATERIAL_NONE,
      MATERIAL_SHININESS_NO, MATERIAL_NONE);

    draw_sphere (-1.25f, 3.0f, 0.0f,
      MATERIAL_NONE, MATERIAL_DIFFUSE, MATERIAL_SPECULAR,
      MATERIAL_SHININESS_LOW, MATERIAL_NONE);

    draw_sphere (1.25f, 3.0f, 0.0f,
      MATERIAL_NONE, MATERIAL_DIFFUSE, MATERIAL_SPECULAR,
      MATERIAL_SHININESS_HIGH, MATERIAL_NONE);

    draw_sphere (3.75f, 3.0f, 0.0f,
      MATERIAL_NONE, MATERIAL_DIFFUSE, MATERIAL_NONE,
      MATERIAL_SHININESS_NO, MATERIAL_EMISSION);

    draw_sphere (-3.75f, 0.0f, 0.0f, 
      MATERIAL_AMBIENT_GREY, MATERIAL_DIFFUSE, MATERIAL_NONE,
      MATERIAL_SHININESS_NO, MATERIAL_NONE);

    draw_sphere (-1.25f, 0.0f, 0.0f,
      MATERIAL_AMBIENT_GREY, MATERIAL_DIFFUSE, MATERIAL_SPECULAR,
      MATERIAL_SHININESS_LOW, MATERIAL_NONE);

    draw_sphere (1.25f, 0.0f, 0.0f,
      MATERIAL_AMBIENT_GREY, MATERIAL_DIFFUSE, MATERIAL_SPECULAR,
      MATERIAL_SHININESS_HIGH, MATERIAL_NONE);

    draw_sphere (3.75f, 0.0f, 0.0f,
      MATERIAL_AMBIENT_GREY, MATERIAL_DIFFUSE, MATERIAL_NONE,
      MATERIAL_SHININESS_NO, MATERIAL_EMISSION);

    draw_sphere (-3.75f, -3.0f, 0.0f, 
      MATERIAL_AMBIENT_COLOR, MATERIAL_DIFFUSE, MATERIAL_NONE,
      MATERIAL_SHININESS_NO, MATERIAL_NONE);

    draw_sphere (-1.25f, -3.0f, 0.0f,
      MATERIAL_AMBIENT_COLOR, MATERIAL_DIFFUSE, MATERIAL_SPECULAR,
      MATERIAL_SHININESS_LOW, MATERIAL_NONE);

    draw_sphere (1.25f, -3.0f, 0.0f,
      MATERIAL_AMBIENT_COLOR, MATERIAL_DIFFUSE, MATERIAL_SPECULAR,
      MATERIAL_SHININESS_HIGH, MATERIAL_NONE);

    draw_sphere (3.75f, -3.0f, 0.0f,
      MATERIAL_AMBIENT_COLOR, MATERIAL_DIFFUSE, MATERIAL_NONE,
      MATERIAL_SHININESS_NO, MATERIAL_EMISSION);

    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();

    double wh = (float) width / (float) height;
    double hw = (float) height / (float) width;
    
    if (width <= height)
      gl.ortho (-4.0, 4.0, -4.0*hw, 4.0*hw, -4.0, 4.0);
    else
      gl.ortho (-4.0*wh, 4.5*wh, -4.0, 4.0, -4.0, 4.0);

    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
  }


  private void init_light () {
    gl.enable (GL.LIGHT0);
    gl.enable (GL.LIGHTING);

    gl.lightfv (GL.LIGHT0, GL.AMBIENT, LIGHT_AMBIENT);
    gl.lightfv (GL.LIGHT0, GL.DIFFUSE, LIGHT_DIFFUSE);
    gl.lightfv (GL.LIGHT0, GL.POSITION, LIGHT_POSITION);
    gl.lightfv (GL.LIGHT0, GL.SPECULAR, LIGHT_SPECULAR);

    gl.light_modelfv (GL.LIGHT_MODEL_AMBIENT, LIGHT_MODEL_AMBIENT);
    gl.light_modelf (GL.LIGHT_MODEL_LOCAL_VIEWER, 
      LIGHT_MODEL_LOCAL_VIEWER);
  }


  public static void main (String [] args) {
    new ColorLight (args).exec ();
  }
}
