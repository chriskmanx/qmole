package gnu.x11;


/** X pixmap. */
public class Pixmap extends Drawable {
  /** 
   * Predefined pixmap.
   *
   * @see Window#NONE
   */
  public static final Pixmap NONE = new Pixmap (0);
  public static final Pixmap COPY_FROM_PARENT = NONE;
  public static final Pixmap PARENT_RELATIVE = new Pixmap (1);


  /** Predefined. */
  public Pixmap (int id) {
    super (id);
  }


  /** X pixmap format. */
  public static class Format  {
    public int depth;
    public int bits_per_pixel;
    public int scanline_pad;

    /**
     * Creates a new instance by reading the data from the X server connection.
     *
     * @param in the input stream to read from
     */
    public Format (ResponseInputStream in) {
      depth = in.read_int8 ();
      bits_per_pixel = in.read_int8 ();
      scanline_pad = in.read_int8 ();
      in.skip (5); // Unused.
    }

    public int bits_per_pixel() {
      return bits_per_pixel;
    }

    public int scanline_pad() {
      return scanline_pad;
    }

    public String toString () {
      return "#Pixmap.Format"
        + "\n  depth: " + depth
        + "\n  bits-per-pixel: " + bits_per_pixel
        + "\n  scanline-pad: " + scanline_pad;
    }
  }


  // opcode 53 - create pixmap
  /** 
   * @see <a href="XCreatePixmap.html">XCreatePixmap</a>
   */
  public Pixmap (Drawable drawable, int width, int height, int depth) {
    super (drawable.display);
    this.width = width;
    this.height = height;

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (53, depth, 4);
      o.write_int32 (id);
      o.write_int32 (drawable.id);
      o.write_int16 (width);
      o.write_int16 (height);
      o.send ();
    }
  }


  /**
   * @see #Pixmap(Drawable, int, int, int)
   */
  public Pixmap (Display display, int width, int height) {
    this (display.default_root, width, height, display.default_depth);      
  }


  /**
   * @see #Pixmap(Drawable, int, int, int)
   */
  public Pixmap (Drawable drawable, int depth) {
    this (drawable, drawable.width, drawable.height, depth);
  }


  /**
   * @see #Pixmap(Drawable, int)
   */
  public Pixmap (Drawable drawable) {
    this (drawable, drawable.display.default_depth);
  }


  // opcode 54 - free pixmap
  /**
   * @see <a href="XFreePixmap.html">XFreePixmap</a>
   */
  public void free () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (54, 0, 2);
      o.write_int32 (id);
      o.send ();
    }
  }
}
