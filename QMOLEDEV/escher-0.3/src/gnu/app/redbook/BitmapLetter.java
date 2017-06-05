package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Draw a bitmapped letter F on the screen for several times. To
 * demonstrate {@link GL#bitmap}. Modified from <code>drawf.c</code>.
 */
public class BitmapLetter extends gnu.x11.extension.glx.Application {
  // start from low-left corner --> inverted F
  private byte [] LETTER = {
    (byte) 0xc0, (byte) 0x00,   // x x 0 0 0 0 0 0
    (byte) 0xc0, (byte) 0x00,   // x x 0 0 0 0 0 0
    (byte) 0xc0, (byte) 0x00,   // x x 0 0 0 0 0 0
    (byte) 0xc0, (byte) 0x00,   // x x 0 0 0 0 0 0
    (byte) 0xc0, (byte) 0x00,   // x x 0 0 0 0 0 0
    (byte) 0xff, (byte) 0x00,   // x x x x 0 0 0 0
    (byte) 0xff, (byte) 0x00,   // x x x x 0 0 0 0
    (byte) 0xc0, (byte) 0x00,   // x x 0 0 0 0 0 0
    (byte) 0xc0, (byte) 0x00,   // x x 0 0 0 0 0 0
    (byte) 0xc0, (byte) 0x00,   // x x 0 0 0 0 0 0 
    (byte) 0xff, (byte) 0xc0,   // x x x x x x 0 0
    (byte) 0xff, (byte) 0xc0    // x x x x x x 0 0
  };


  public BitmapLetter (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "bitmapped letter F",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
    init_window (100, 100);

    gl.pixel_storei (GL.UNPACK_ALIGNMENT, 1);
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);
    gl.raster_pos2i (20, 20);

    gl.bitmap (10, 12, 0.0f, 0.0f, 11.0f, 0.0f, LETTER);
    gl.bitmap (10, 12, 0.0f, 0.0f, 11.0f, 0.0f, LETTER);
    gl.bitmap (10, 12, 0.0f, 0.0f, 11.0f, 0.0f, LETTER);

    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();   
    gl.ortho (0.0, width, 0.0, height, -1.0, 1.0);
    gl.matrix_mode (GL.MODELVIEW);
  }


  // @screenshot gif
  public static void main (String [] args) {
    new BitmapLetter (args).exec ();
  }
}
