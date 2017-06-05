package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Mask non-rectangular region with stencil buffer. It draws two rotated
 * tori in a window. A diamond in the center of the window masks out part
 * of the scene. Within this mask, a different model (a sphere) is drawn in
 * a different color. Modified from <code>stencil.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/NonRectangular.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/NonRectangular.help">
 * help output</a>
 */
public class NonRectangular extends gnu.x11.extension.glx.Application {
  private static final float [] BLUE_DIFFUSE = {0.1f, 0.1f, 0.7f, 1.0f};
  private static final float [] BLUE_SPECULAR = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float [] YELLOW_DIFFUSE = {0.7f, 0.7f, 0.0f, 1.0f};
  private static final float [] YELLOW_SPECULAR = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float [] LIGHT_POSITION = {1.0f, 1.0f, 1.0f, 0.0f};


  public NonRectangular (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "mask non-rectangular region",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    visual_config.set_depth_size (1);
    visual_config.set_stencil_size (1);
    init_window (400, 400);

    gl.enable (GL.STENCIL_TEST);
    init_light ();

    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
    gl.translatef (0.0f, 0.0f, -5.0f);
  }


  private void draw_diamond () {
    gl.matrix_mode (GL.PROJECTION);
    gl.push_matrix ();
    gl.load_identity ();
    gl.ortho (-3.0, 3.0, -3.0, 3.0, -1.0, 1.0);

    gl.matrix_mode (GL.MODELVIEW);
    gl.push_matrix ();
    gl.load_identity ();

    // disable color buffer update
    gl.color_mask (false, false, false, false);
    gl.disable (GL.DEPTH_TEST);
    gl.stencil_func (GL.ALWAYS, 0x1, 0x1);
    gl.stencil_op (GL.REPLACE, GL.REPLACE, GL.REPLACE);

    gl.begin (GL.QUADS);
    gl.vertex3f (-1.0f, 0.0f, 0.0f);
    gl.vertex3f (0.0f, 1.0f, 0.0f);
    gl.vertex3f (1.0f, 0.0f, 0.0f);
    gl.vertex3f (0.0f, -1.0f, 0.0f);
    gl.end ();

    gl.pop_matrix ();

    gl.matrix_mode (GL.PROJECTION);
    gl.pop_matrix ();

    gl.matrix_mode (GL.MODELVIEW);
    
    // enable color buffer update
    gl.color_mask (true, true, true, true);
    gl.enable (GL.DEPTH_TEST);
    gl.stencil_op (GL.KEEP, GL.KEEP, GL.KEEP);
  }


  private void draw_sphere () {
    gl.materialfv (GL.FRONT, GL.DIFFUSE, BLUE_DIFFUSE);
    gl.materialfv (GL.FRONT, GL.SPECULAR, BLUE_SPECULAR);
    gl.materialf (GL.FRONT, GL.SHININESS, 45.0f);
    glut.solid_sphere (0.5, 15, 15);
  }


  private void draw_tori () {
    gl.materialfv (GL.FRONT, GL.DIFFUSE, YELLOW_DIFFUSE);
    gl.materialfv (GL.FRONT, GL.SPECULAR, YELLOW_SPECULAR);
    gl.materialf (GL.FRONT, GL.SHININESS, 64.0f);

    // first torus
    gl.push_matrix ();
    gl.rotatef (45.0f, 0.0f, 0.0f, 1.0f);
    gl.rotatef (45.0f, 0.0f, 1.0f, 0.0f);
    glut.solid_torus (0.275, 0.85, 15, 15);

    // second torus
    gl.push_matrix ();
    gl.rotatef (90.0f, 1.0f, 0.0f, 0.0f);
    glut.solid_torus (0.275, 0.85, 15, 15);
    gl.pop_matrix ();
    gl.pop_matrix ();
  }


  protected void handle_expose () {
    gl.clear (GL.STENCIL_BUFFER_BIT);
    draw_diamond ();

    gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);
    gl.stencil_func (GL.EQUAL, 0x1, 0x1);
    draw_sphere ();
    gl.stencil_func (GL.NOTEQUAL, 0x1, 0x1);
    draw_tori ();

    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();
    double wh = (float) width / (float) height;
    glu.perspective (45.0,  wh, 3.0, 7.0);
    gl.matrix_mode (GL.MODELVIEW);
  }


  private void init_light () {
    gl.enable (GL.LIGHT0);
    gl.enable (GL.LIGHTING);

    gl.lightfv (GL.LIGHT0, GL.POSITION, LIGHT_POSITION);
  }


  public static void main (String [] args) {
    new NonRectangular (args).exec ();
  }
}
