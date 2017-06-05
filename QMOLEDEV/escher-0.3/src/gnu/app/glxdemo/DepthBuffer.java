package gnu.app.glxdemo;

import gnu.x11.extension.glx.GL;


/**
 * Draw with depth buffer. To toggle anti-aliasing, press '1'. To toggle
 * stippling, press '2'. Modified from <code>depth.c</code> in <a href=
 * "http://trant.sgi.com/opengl/examples/samples/samples.html">
 * opengl sample</a> by SGI.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/DepthBuffer.gif">
 * screenshot 4</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/DepthBuffer.help">
 * help output</a>
 */
public class DepthBuffer extends gnu.x11.extension.glx.Application {
  private static final byte [] HALFTONE = {
    (byte) 0xaa, (byte) 0xaa, (byte) 0xaa, (byte) 0xaa,
    (byte) 0x55, (byte) 0x55, (byte) 0x55, (byte) 0x55,
    (byte) 0xaa, (byte) 0xaa, (byte) 0xaa, (byte) 0xaa,
    (byte) 0x55, (byte) 0x55, (byte) 0x55, (byte) 0x55, 
    (byte) 0xaa, (byte) 0xaa, (byte) 0xaa, (byte) 0xaa,
    (byte) 0x55, (byte) 0x55, (byte) 0x55, (byte) 0x55,
    (byte) 0xaa, (byte) 0xaa, (byte) 0xaa, (byte) 0xaa,
    (byte) 0x55, (byte) 0x55, (byte) 0x55, (byte) 0x55, 
    (byte) 0xaa, (byte) 0xaa, (byte) 0xaa, (byte) 0xaa,
    (byte) 0x55, (byte) 0x55, (byte) 0x55, (byte) 0x55, 
    (byte) 0xaa, (byte) 0xaa, (byte) 0xaa, (byte) 0xaa,
    (byte) 0x55, (byte) 0x55, (byte) 0x55, (byte) 0x55,
    (byte) 0xaa, (byte) 0xaa, (byte) 0xaa, (byte) 0xaa,
    (byte) 0x55, (byte) 0x55, (byte) 0x55, (byte) 0x55, 
    (byte) 0xaa, (byte) 0xaa, (byte) 0xaa, (byte) 0xaa,
    (byte) 0x55, (byte) 0x55, (byte) 0x55, (byte) 0x55, 
    (byte) 0xaa, (byte) 0xaa, (byte) 0xaa, (byte) 0xaa,
    (byte) 0x55, (byte) 0x55, (byte) 0x55, (byte) 0x55,
    (byte) 0xaa, (byte) 0xaa, (byte) 0xaa, (byte) 0xaa,
    (byte) 0x55, (byte) 0x55, (byte) 0x55, (byte) 0x55, 
    (byte) 0xaa, (byte) 0xaa, (byte) 0xaa, (byte) 0xaa,
    (byte) 0x55, (byte) 0x55, (byte) 0x55, (byte) 0x55, 
    (byte) 0xaa, (byte) 0xaa, (byte) 0xaa, (byte) 0xaa,
    (byte) 0x55, (byte) 0x55, (byte) 0x55, (byte) 0x55,
    (byte) 0xaa, (byte) 0xaa, (byte) 0xaa, (byte) 0xaa,
    (byte) 0x55, (byte) 0x55, (byte) 0x55, (byte) 0x55, 
    (byte) 0xaa, (byte) 0xaa, (byte) 0xaa, (byte) 0xaa,
    (byte) 0x55, (byte) 0x55, (byte) 0x55, (byte) 0x55, 
    (byte) 0xaa, (byte) 0xaa, (byte) 0xaa, (byte) 0xaa,
    (byte) 0x55, (byte) 0x55, (byte) 0x55, (byte) 0x55,
    (byte) 0xaa, (byte) 0xaa, (byte) 0xaa, (byte) 0xaa,
    (byte) 0x55, (byte) 0x55, (byte) 0x55, (byte) 0x55
  };


  private boolean anti_aliasing, stippling;


  public DepthBuffer (String [] args) {
    super (args, KEYBOARD_BIT | RESIZE_BIT);

    about ("0.1", "depth buffer",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo toggle anti-aliasing, press '1'."
      + "\nTo toggle stippling, press '2'.");

    if (help_option) return;

    visual_config.set_depth_size (1);
    init_window (300, 300);

    gl.polygon_stipple (HALFTONE);
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);

    if (anti_aliasing) {
      gl.blend_func (GL.SRC_ALPHA, GL.ONE);
      gl.enable (GL.BLEND);
      gl.enable (GL.POLYGON_SMOOTH);
      gl.disable (GL.DEPTH_TEST);

    } else {
      gl.disable (GL.BLEND);
      gl.disable (GL.POLYGON_SMOOTH);
      gl.enable (GL.DEPTH_TEST);
    }


    if (stippling) 
      gl.enable (GL.POLYGON_STIPPLE);
    else
      gl.disable (GL.POLYGON_STIPPLE);
      
    gl.begin (GL.TRIANGLES);
    gl.color3f (0.0f, 0.0f, 1.0f);
    gl.vertex3f ( 0.9f, -0.9f, -30.0f);
    gl.vertex3f ( 0.9f,  0.9f, -30.0f);
    gl.vertex3f (-0.9f,  0.0f, -30.0f);

    gl.color3f (0.0f, 1.0f, 0.0f);
    gl.vertex3f (-0.9f, -0.9f, -40.0f);
    gl.vertex3f (-0.9f,  0.9f, -40.0f);
    gl.vertex3f ( 0.9f,  0.0f, -25.0f);
    gl.end ();
    
    gl.swap_buffers (window);
  }


  protected void handle_keyboard (int key, int state, int x, int y) {
    switch (key) {
    case '1': anti_aliasing = !anti_aliasing; break;
    case '2': stippling = !stippling; break;
    default: return;
    }      

    mark_window_dirty ();
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();
    gl.ortho (-1.0, 1.0, -1.0, 1.0, -0.5, 1000.0);    
    gl.matrix_mode (GL.MODELVIEW);
  }


  public static void main (String [] args) {
    new DepthBuffer (args).exec ();
  }
}
