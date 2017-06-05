package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Draw two overlapping filled triangles. To demonstrate the effect of
 * order on alpha blending. To toggle the order of drawing, press 't' or
 * 'T'. Modified from <code>alpha.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/AlphaOrder.gif">
 * screenshot 2</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/AlphaOrder.help">
 * help output</a>
 */
public class AlphaOrder extends gnu.x11.extension.glx.Application {
  private boolean left_first = true;


  public AlphaOrder (String [] args) {
    super (args, KEYBOARD_BIT | RESIZE_BIT);

    about ("0.1", "order of alpha blending",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo toggle order of drawing, press 't' or 'T'.");

    if (help_option) return;
    init_window (200, 200);

    gl.enable (GL.BLEND);
    gl.blend_func (GL.SRC_ALPHA, GL.ONE_MINUS_SRC_ALPHA);
    gl.shade_model (GL.FLAT);
  }


  private void draw_left_triangle () {
   gl.begin (GL.TRIANGLES);
   gl.color4f (1.0f, 1.0f, 0.0f, 0.75f); // yellow
   gl.vertex3f (0.1f, 0.9f, 0.0f); 
   gl.vertex3f (0.1f, 0.1f, 0.0f); 
   gl.vertex3f (0.7f, 0.5f, 0.0f); 
   gl.end ();
  }


  private void draw_right_triangle () {
    gl.begin (GL.TRIANGLES);
    gl.color4f (0.0f, 1.0f, 1.0f, 0.75f); // cyan
    gl.vertex3f (0.9f, 0.9f, 0.0f);
    gl.vertex3f (0.3f, 0.5f, 0.0f);
    gl.vertex3f (0.9f, 0.1f, 0.0f);
    gl.end ();
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);
    
    if (left_first) {
      draw_left_triangle ();
      draw_right_triangle ();
    } else {
      draw_right_triangle ();
      draw_left_triangle ();
    }      
    
    gl.swap_buffers (window);
  }


  protected void handle_keyboard (int key, int state, int x, int y) {
    if (key == 't' || key == 'T') {
      left_first = !left_first;
      mark_window_dirty ();
    }
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();

    double wh = (float) width / (float) height;
    double hw = (float) height / (float) width;
    
    if (width <= height)
      glu.ortho_2d (0.0, 1.0, 0.0, 1.0*hw);
    else
      glu.ortho_2d (0.0, 1.0*wh, 0.0, 1.0);

    gl.matrix_mode (GL.MODELVIEW);
  }


  public static void main (String [] args) {
    new AlphaOrder (args).exec ();
  }
}
