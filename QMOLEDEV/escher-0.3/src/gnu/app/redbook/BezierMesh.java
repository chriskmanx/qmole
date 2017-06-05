package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Render a lighted, filled Bezier surface. To demonstrate two-dimensional
 * evaluators. Modified from <code>bezmesh.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/BezierMesh.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/BezierMesh.help">
 * help output</a>
 */
public class BezierMesh extends gnu.x11.extension.glx.Application {
  private static final float [] [] [] CONTROL_POINTS = {
    {{-1.5f, -1.5f, 4.0f},
     {-0.5f, -1.5f, 2.0f},
     {0.5f, -1.5f, -1.0f},
     {1.5f, -1.5f, 2.0f}},

    {{-1.5f, -0.5f, 1.0f},
     {-0.5f, -0.5f, 3.0f},
     {0.5f, -0.5f, 0.0f},
     {1.5f, -0.5f, -1.0f}},

    {{-1.5f, 0.5f, 4.0f},
     {-0.5f, 0.5f, 0.0f},
     {0.5f, 0.5f, 3.0f},
     {1.5f, 0.5f, 4.0f}}, 

    {{-1.5f, 1.5f, -2.0f},
     {-0.5f, 1.5f, -2.0f},
     {0.5f, 1.5f, 0.0f},
     {1.5f, 1.5f, -1.0f}}
  };


  private static final float [] LIGHT_AMBIENT
    = {0.2f, 0.2f, 0.2f, 1.0f};
  private static final float [] LIGHT_POSITION
    = {0.0f, 0.0f, 2.0f, 1.0f};
  private static final float [] MATERIAL_DIFFUSE 
    = {0.6f, 0.6f, 0.6f, 1.0f};
  private static final float [] MATERIAL_SPECULAR 
    = {1.0f, 1.0f, 1.0f, 1.0f};
  private static final float MATERIAL_SHININESS = 50.0f;


  public BezierMesh (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "bezier mesh",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    visual_config.set_depth_size (1);
    init_window (500, 500);

    gl.enable (GL.AUTO_NORMAL);
    gl.enable (GL.DEPTH_TEST);
    gl.enable (GL.MAP2_VERTEX_3);

    gl.map2f (GL.MAP2_VERTEX_3, 0.0f, 1.0f, 3, 4, 
      0.0f, 1.0f, 12, 4, gnu.util.Misc.linearize (CONTROL_POINTS));
    gl.map_grid2f (20, 0.0f, 1.0f, 20, 0.0f, 1.0f);

    init_light ();
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);
    gl.push_matrix ();
    gl.rotatef (85.0f, 1.0f, 1.0f, 1.0f);
    gl.eval_mesh2 (GL.FILL, 0, 20, 0, 20);
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
      gl.ortho (-4.0, 4.0, -4.0*hw, 4.0*hw, -4.0, 4.0);
    else
      gl.ortho (-4.0*wh, 4.0*wh, -4.0, 4.0, -4.0, 4.0);

    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
  }


  private void init_light () {
    gl.enable (GL.LIGHTING);
    gl.enable (GL.LIGHT0);

    gl.lightfv (GL.LIGHT0, GL.AMBIENT, LIGHT_AMBIENT);
    gl.lightfv (GL.LIGHT0, GL.POSITION, LIGHT_POSITION);

    gl.materialfv (GL.FRONT, GL.DIFFUSE, MATERIAL_DIFFUSE);
    gl.materialfv (GL.FRONT, GL.SPECULAR, MATERIAL_SPECULAR);
    gl.materialf (GL.FRONT, GL.SHININESS, MATERIAL_SHININESS);
  }


  public static void main (String [] args) {
    new BezierMesh (args).exec ();
  }
}
