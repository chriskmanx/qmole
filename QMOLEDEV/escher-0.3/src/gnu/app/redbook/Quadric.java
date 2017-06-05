package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Test quadric objects. Modified from <code>quadric.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Quadric.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Quadric.help">
 * help output</a>
 */
public class Quadric extends gnu.x11.extension.glx.Application {
  private static final float [] LIGHT_POSITION = {1.0f, 1.0f, 1.0f, 0.0f};
  private static final float MATERIAL_SHININESS = 50.0f;

  private static final float [] LIGHT_MODEL_AMBIENT
    = {0.5f, 0.5f, 0.5f, 1.0f};
  private static final float [] MATERIAL_AMBIENT
    = {0.5f, 0.5f, 0.5f, 1.0f};
  private static final float [] MATERIAL_SPECULAR
    = {1.0f, 1.0f, 1.0f, 1.0f};


  private int display_list;


  public Quadric (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "lighting model with color",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    visual_config.set_depth_size (1);
    init_window (500, 500);

    gl.enable (GL.DEPTH_TEST);
    init_light ();
    init_object ();
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);
    gl.push_matrix ();

    gl.enable (GL.LIGHTING);
    gl.shade_model (GL.SMOOTH);
    gl.translatef (-1.0f, -1.0f, 0.0f);
    gl.call_list (display_list);

    gl.shade_model (GL.FLAT);
    gl.translatef (0.0f, 2.0f, 0.0f);
    gl.push_matrix ();
    gl.rotatef (300.0f, 1.0f, 0.0f, 0.0f);
    gl.call_list (display_list+1);
    gl.pop_matrix ();

    gl.disable (GL.LIGHTING);
    gl.color3f (0.0f, 1.0f, 1.0f);
    gl.translatef (2.0f, -2.0f, 0.0f);
    gl.call_list (display_list+2);

    gl.color3f (1.0f, 1.0f, 0.0f);
    gl.translatef (0.0f, 2.0f, 0.0f);
    gl.call_list (display_list+3);

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
    gl.enable (GL.LIGHT0);
    gl.enable (GL.LIGHTING);

    gl.lightfv (GL.LIGHT0, GL.POSITION, LIGHT_POSITION);
    gl.light_modelfv (GL.LIGHT_MODEL_AMBIENT, LIGHT_MODEL_AMBIENT);

    gl.materialfv (GL.FRONT, GL.AMBIENT, MATERIAL_AMBIENT);
    gl.materialf (GL.FRONT, GL.SHININESS, MATERIAL_SHININESS);
    gl.materialfv (GL.FRONT, GL.SPECULAR, MATERIAL_SPECULAR);
  }


  private void init_object () {
    // different drawing styles and surface normal specification

    display_list = gl.gen_lists (4);
    gnu.x11.extension.glx.Quadric quadric = new gnu.x11.extension.glx.Quadric (gl);

    // smooth shaded
    quadric.draw_style = gnu.x11.extension.glx.Quadric.FILL;
    quadric.normals = gnu.x11.extension.glx.Quadric.SMOOTH;
    gl.new_list (display_list, GL.COMPILE);
    quadric.sphere (0.75, 15, 10);
    gl.end_list ();

    // flat shaded
    quadric.draw_style = gnu.x11.extension.glx.Quadric.FILL;
    quadric.normals = gnu.x11.extension.glx.Quadric.FLAT;
    gl.new_list (display_list+1, GL.COMPILE);
    quadric.cylinder (0.5, 0.3, 1.0, 15, 5);
    gl.end_list ();

    // all polygon wireframe
    quadric.draw_style = gnu.x11.extension.glx.Quadric.LINE;
    quadric.normals = gnu.x11.extension.glx.Quadric.NONE;
    gl.new_list (display_list+2, GL.COMPILE);
    quadric.disk (0.25, 1.0, 20, 4);
    gl.end_list ();

    // boundary only
    quadric.draw_style = gnu.x11.extension.glx.Quadric.SILHOUETTE;
    quadric.normals = gnu.x11.extension.glx.Quadric.NONE;
    gl.new_list (display_list+3, GL.COMPILE);
    quadric.partial_disk (0.0, 1.0, 20, 4, 0.0, 225.0);
    gl.end_list ();
  }


  public static void main (String [] args) {
    new Quadric (args).exec ();
  }
}
