package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Draw a Bezier surface. Modified from <code>bezsurf.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/BezierSurface.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/BezierSurface.help">
 * help output</a>
 */
public class BezierSurface extends gnu.x11.extension.glx.Application {
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


  public BezierSurface (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "bezier surface",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    visual_config.set_depth_size (1);
    init_window (500, 500);

    gl.enable (GL.DEPTH_TEST);
    gl.enable (GL.MAP2_VERTEX_3);
    gl.shade_model (GL.FLAT);

    gl.map2f (GL.MAP2_VERTEX_3, 0.0f, 1.0f, 3, 4, 
      0.0f, 1.0f, 12, 4, gnu.util.Misc.linearize (CONTROL_POINTS));
    gl.map_grid2f (20, 0.0f, 1.0f, 20, 0.0f, 1.0f);

  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);

    gl.push_matrix ();
    gl.rotatef (85.0f, 1.0f, 1.0f, 1.0f);
    for (int j=0; j<=8; j++) {
      gl.begin (GL.LINE_STRIP);
      for (int i=0; i<=30; i++) 
        gl.eval_coord2f (i/30.0f, j/8.0f);
      gl.end ();

      gl.begin (GL.LINE_STRIP);
      for (int i=0; i<=30; i++)
        gl.eval_coord2f (j/8.0f, i/30.0f);
      gl.end ();
    }

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


  public static void main (String [] args) {
    new BezierSurface (args).exec ();
  }
}
