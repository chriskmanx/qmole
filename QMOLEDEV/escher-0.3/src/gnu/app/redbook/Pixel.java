package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Draw a checkerbard image with pixel operations. To demonstrate {@link
 * GL#draw_pixels}, {@link GL#copy_pixels}, and {@link GL#pixel_zoom}. move
 * pointer while pressing BUTTON1.
 *
 * <ul>
 * <li>To draw,
 * <li>To increase zoom factor, press 'z'.
 * <li>To decrease zoom factor, press 'Z'.
 * <li>To reset, press 'r' or 'R'.
 * </ul> Modified from <code>image.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Pixel.gif">
 * screenshot 4</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Pixel.help">
 * help output</a>
 */
public class Pixel extends gnu.x11.extension.glx.Application {
  private float zoom_factor = 1.0f;


  public Pixel (String [] args) {
    super (args, BUTTON1_MOTION_BIT | KEYBOARD_BIT | RESIZE_BIT);

    about ("0.1", "pixel operations",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo draw, move pointer while pressing BUTTON1."
      + "\nTo increase zoom factor, press 'z'."
      + "\nTo decrease zoom factor, press 'Z'."
      + "\nTo reset, press 'r' or 'R'.");

    if (help_option) return;
    init_window (250, 250);

    gl.shade_model (GL.FLAT);
    gl.pixel_storei (GL.UNPACK_ALIGNMENT, 1);
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);

    gl.raster_pos2i (0, 0);
    gl.draw_pixels (Checkerboard.SIZE, Checkerboard.SIZE, GL.RGB,
      GL.UNSIGNED_BYTE, Checkerboard.RGB_PIXELS);

    gl.swap_buffers (window);
  }


  protected void handle_keyboard (int key, int state, int x, int y) {
    switch (key) {
    case 'r':                   // fall through
    case 'R':
      zoom_factor = 1.0f;
      System.out.println ("zoom-factor reset to 1.0");
      mark_window_dirty ();
      break;

    case 'z':
      zoom_factor += 0.5f;
      if (zoom_factor > 3.0f) zoom_factor = 3.0f;
      System.out.println ("zoom-factor: " + zoom_factor);
      break;

    case 'Z':
      zoom_factor -= 0.5f;
      if (zoom_factor < 0.5f) zoom_factor = 0.5f;
      System.out.println ("zoom-factor: " + zoom_factor);
      break;
    }
  }


  protected void handle_motion (int state, int x, int y) {
    int screen_y = window.height - y;
    gl.raster_pos2i (x, screen_y);

    gl.pixel_zoom (zoom_factor, zoom_factor);
    gl.copy_pixels (0, 0, Checkerboard.SIZE, Checkerboard.SIZE, GL.COLOR);
    gl.pixel_zoom (1.0f, 1.0f);

    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();
    glu.ortho_2d (0.0, width, 0.0, height);
    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
  }


  public static void main (String [] args) {
    new Pixel (args).exec ();
  }
}
