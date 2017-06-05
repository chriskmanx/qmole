package gnu.app.glxdemo;

import gnu.x11.extension.glx.GL;


/**
 * Draw with stencil buffer. Modified from <code>stencil.c</code> in <a href=
 * "http://trant.sgi.com/opengl/examples/samples/samples.html">
 * opengl sample</a> by SGI.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/StencilBuffer.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/StencilBuffer.help">
 * help output</a>
 */
public class StencilBuffer extends gnu.x11.extension.glx.Application {
  public StencilBuffer (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "stencil buffer",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    visual_config.set_stencil_size (1);
    visual_config.set_double_buffer ();
    init_window (300, 300);

    gl.enable (GL.STENCIL_TEST);
    gl.stencil_mask (1);
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT | GL.STENCIL_BUFFER_BIT);

    gl.stencil_func (GL.ALWAYS, 1, 1);
    gl.stencil_op (GL.KEEP, GL.KEEP, GL.REPLACE);

    gl.color3ub ((byte) 200, (byte) 0, (byte) 0);
    gl.begin (GL.POLYGON);        
    gl.vertex3i (-4, -4, 0);
    gl.vertex3i ( 4, -4, 0);
    gl.vertex3i ( 0,  4, 0);
    gl.end ();

    gl.stencil_func (GL.EQUAL, 1, 1);
    gl.stencil_op (GL.INCR, GL.KEEP, GL.DECR);

    gl.color3ub ((byte) 0, (byte) 200, (byte) 0);
    gl.begin (GL.POLYGON);
    gl.vertex3i (3, 3, 0);
    gl.vertex3i (-3, 3, 0);
    gl.vertex3i (-3, -3, 0);
    gl.vertex3i (3, -3, 0);
    gl.end ();

    gl.stencil_func (GL.EQUAL, 1, 1);
    gl.stencil_op (GL.KEEP, GL.KEEP, GL.KEEP);

    gl.color3ub ((byte) 0, (byte) 0, (byte) 200);
    gl.begin (GL.POLYGON);
    gl.vertex3i (3, 3, 0);
    gl.vertex3i (-3, 3, 0);
    gl.vertex3i (-3, -3, 0);
    gl.vertex3i (3, -3, 0);
    gl.end ();

    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.ortho (-5.0, 5.0, -5.0, 5.0, -5.0, 5.0);
    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
  }


  public static void main (String [] args) {
    new StencilBuffer (args).exec ();
  }
}
