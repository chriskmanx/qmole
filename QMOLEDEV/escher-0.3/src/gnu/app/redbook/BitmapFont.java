package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Draw some texts in a bitmapped font. To demonstrate {@link GL#bitmap},
 * other pixel routines, and the use of display lists. Modified from
 * <code>font.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/BitmapFont.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/BitmapFont.help">
 * help output</a>
 */
public class BitmapFont extends gnu.x11.extension.glx.Application {
  private byte [] SPACE = new byte [13]; // all zeros

  private byte [] [] LETTERS = {
    {(byte) 0x00, (byte) 0x00, (byte) 0xc3, (byte) 0xc3, 
     (byte) 0xc3, (byte) 0xc3, (byte) 0xff, (byte) 0xc3, 
     (byte) 0xc3, (byte) 0xc3, (byte) 0x66, (byte) 0x3c, (byte) 0x18},

    {(byte) 0x00, (byte) 0x00, (byte) 0xfe, (byte) 0xc7, 
     (byte) 0xc3, (byte) 0xc3, (byte) 0xc7, (byte) 0xfe, 
     (byte) 0xc7, (byte) 0xc3, (byte) 0xc3, (byte) 0xc7, (byte) 0xfe},

    {(byte) 0x00, (byte) 0x00, (byte) 0x7e, (byte) 0xe7, 
     (byte) 0xc0, (byte) 0xc0, (byte) 0xc0, (byte) 0xc0, 
     (byte) 0xc0, (byte) 0xc0, (byte) 0xc0, (byte) 0xe7, (byte) 0x7e},

    {(byte) 0x00, (byte) 0x00, (byte) 0xfc, (byte) 0xce, 
     (byte) 0xc7, (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, 
     (byte) 0xc3, (byte) 0xc3, (byte) 0xc7, (byte) 0xce, (byte) 0xfc},

    {(byte) 0x00, (byte) 0x00, (byte) 0xff, (byte) 0xc0, 
     (byte) 0xc0, (byte) 0xc0, (byte) 0xc0, (byte) 0xfc, 
     (byte) 0xc0, (byte) 0xc0, (byte) 0xc0, (byte) 0xc0, (byte) 0xff},

    {(byte) 0x00, (byte) 0x00, (byte) 0xc0, (byte) 0xc0, 
     (byte) 0xc0, (byte) 0xc0, (byte) 0xc0, (byte) 0xc0, 
     (byte) 0xfc, (byte) 0xc0, (byte) 0xc0, (byte) 0xc0, (byte) 0xff},

    {(byte) 0x00, (byte) 0x00, (byte) 0x7e, (byte) 0xe7, 
     (byte) 0xc3, (byte) 0xc3, (byte) 0xcf, (byte) 0xc0, 
     (byte) 0xc0, (byte) 0xc0, (byte) 0xc0, (byte) 0xe7, (byte) 0x7e},

    {(byte) 0x00, (byte) 0x00, (byte) 0xc3, (byte) 0xc3, 
     (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, (byte) 0xff, 
     (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, (byte) 0xc3},

    {(byte) 0x00, (byte) 0x00, (byte) 0x7e, (byte) 0x18, 
     (byte) 0x18, (byte) 0x18, (byte) 0x18, (byte) 0x18, 
     (byte) 0x18, (byte) 0x18, (byte) 0x18, (byte) 0x18, (byte) 0x7e},

    {(byte) 0x00, (byte) 0x00, (byte) 0x7c, (byte) 0xee, 
     (byte) 0xc6, (byte) 0x06, (byte) 0x06, (byte) 0x06, 
     (byte) 0x06, (byte) 0x06, (byte) 0x06, (byte) 0x06, (byte) 0x06},

    {(byte) 0x00, (byte) 0x00, (byte) 0xc3, (byte) 0xc6, 
     (byte) 0xcc, (byte) 0xd8, (byte) 0xf0, (byte) 0xe0, 
     (byte) 0xf0, (byte) 0xd8, (byte) 0xcc, (byte) 0xc6, (byte) 0xc3},

    {(byte) 0x00, (byte) 0x00, (byte) 0xff, (byte) 0xc0, 
     (byte) 0xc0, (byte) 0xc0, (byte) 0xc0, (byte) 0xc0, 
     (byte) 0xc0, (byte) 0xc0, (byte) 0xc0, (byte) 0xc0, (byte) 0xc0},

    {(byte) 0x00, (byte) 0x00, (byte) 0xc3, (byte) 0xc3, 
     (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, 
     (byte) 0xdb, (byte) 0xff, (byte) 0xff, (byte) 0xe7, (byte) 0xc3},

    {(byte) 0x00, (byte) 0x00, (byte) 0xc7, (byte) 0xc7, 
     (byte) 0xcf, (byte) 0xcf, (byte) 0xdf, (byte) 0xdb, 
     (byte) 0xfb, (byte) 0xf3, (byte) 0xf3, (byte) 0xe3, (byte) 0xe3},

    {(byte) 0x00, (byte) 0x00, (byte) 0x7e, (byte) 0xe7, 
     (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, 
     (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, (byte) 0xe7, (byte) 0x7e},

    {(byte) 0x00, (byte) 0x00, (byte) 0xc0, (byte) 0xc0, 
     (byte) 0xc0, (byte) 0xc0, (byte) 0xc0, (byte) 0xfe, 
     (byte) 0xc7, (byte) 0xc3, (byte) 0xc3, (byte) 0xc7, (byte) 0xfe},

    {(byte) 0x00, (byte) 0x00, (byte) 0x3f, (byte) 0x6e, 
     (byte) 0xdf, (byte) 0xdb, (byte) 0xc3, (byte) 0xc3, 
     (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, (byte) 0x66, (byte) 0x3c},

    {(byte) 0x00, (byte) 0x00, (byte) 0xc3, (byte) 0xc6, 
     (byte) 0xcc, (byte) 0xd8, (byte) 0xf0, (byte) 0xfe, 
     (byte) 0xc7, (byte) 0xc3, (byte) 0xc3, (byte) 0xc7, (byte) 0xfe},

    {(byte) 0x00, (byte) 0x00, (byte) 0x7e, (byte) 0xe7, 
     (byte) 0x03, (byte) 0x03, (byte) 0x07, (byte) 0x7e, 
     (byte) 0xe0, (byte) 0xc0, (byte) 0xc0, (byte) 0xe7, (byte) 0x7e},

    {(byte) 0x00, (byte) 0x00, (byte) 0x18, (byte) 0x18, 
     (byte) 0x18, (byte) 0x18, (byte) 0x18, (byte) 0x18, 
     (byte) 0x18, (byte) 0x18, (byte) 0x18, (byte) 0x18, (byte) 0xff},

    {(byte) 0x00, (byte) 0x00, (byte) 0x7e, (byte) 0xe7, 
     (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, 
     (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, (byte) 0xc3},

    {(byte) 0x00, (byte) 0x00, (byte) 0x18, (byte) 0x3c, 
     (byte) 0x3c, (byte) 0x66, (byte) 0x66, (byte) 0xc3, 
     (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, (byte) 0xc3},

    {(byte) 0x00, (byte) 0x00, (byte) 0xc3, (byte) 0xe7, 
     (byte) 0xff, (byte) 0xff, (byte) 0xdb, (byte) 0xdb, 
     (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, (byte) 0xc3, (byte) 0xc3},

    {(byte) 0x00, (byte) 0x00, (byte) 0xc3, (byte) 0x66, 
     (byte) 0x66, (byte) 0x3c, (byte) 0x3c, (byte) 0x18, 
     (byte) 0x3c, (byte) 0x3c, (byte) 0x66, (byte) 0x66, (byte) 0xc3},

    {(byte) 0x00, (byte) 0x00, (byte) 0x18, (byte) 0x18, 
     (byte) 0x18, (byte) 0x18, (byte) 0x18, (byte) 0x18, 
     (byte) 0x3c, (byte) 0x3c, (byte) 0x66, (byte) 0x66, (byte) 0xc3},

    {(byte) 0x00, (byte) 0x00, (byte) 0xff, (byte) 0xc0, 
     (byte) 0xc0, (byte) 0x60, (byte) 0x30, (byte) 0x7e, 
     (byte) 0x0c, (byte) 0x06, (byte) 0x03, (byte) 0x03, (byte) 0xff}
  };


  private int display_list_offset;
  

  public BitmapFont (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "bitmap font",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
    init_window (300, 100);

    gl.shade_model (GL.FLAT);
    gl.pixel_storei (GL.UNPACK_ALIGNMENT, 1);
    display_list_offset = gl.gen_lists (128);

    init_letters ();
    init_space ();
  }


  private void font_string (String s) {
    gl.push_attrib (GL.LIST_BIT);
    gl.list_base (display_list_offset);
    gl.call_lists (GL.UNSIGNED_BYTE, s.getBytes ());
    gl.pop_attrib ();
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);

    gl.raster_pos2i (20, 60);
    font_string ("THE QUICK BROWN FOX JUMPS");
    gl.raster_pos2i (20, 40);
    font_string ("OVER A LAZY DOG");

    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();   
    gl.ortho (0.0, width, 0.0, height, -1.0, 1.0);
    gl.matrix_mode (GL.MODELVIEW);
  }


  private void init_letters () {
    for (int i=0; i<26; i++) {
      int j = i + 'A';
      gl.new_list (display_list_offset+j, GL.COMPILE);
      gl.bitmap (8, 13, 0.0f, 2.0f, 10.0f, 0.0f, LETTERS [i]);
      gl.end_list ();
    }
  }
  

  private void init_space () {
    gl.new_list (display_list_offset + ' ', GL.COMPILE);
    gl.bitmap (8, 13, 0.0f, 2.0f, 10.0f, 0.0f, SPACE);
    gl.end_list ();
  }


  public static void main (String [] args) {
    new BitmapFont (args).exec ();
  }
}
