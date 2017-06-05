package gnu.x11.image;

import gnu.x11.Pixmap;


public abstract class Image {

  /**
   * The possible pixmap formats for client side images.
   */
  public static enum Format {
    BITMAP   { public int id() { return 0; } },
    XYPIXMAP { public int id() { return 1; } },
    ZPIXMAP  { public int id() { return 2; } };

    public abstract int id ();
  }

  public static final int LSB_FIRST = 0;
  public static final int MSB_FIRST = 1;
  public static final int LEAST_SIGNIFICANT = 0;
  public static final int MOST_SIGNIFICANT = 1;

  byte [] data;
  int width;
  int height;
  int left_pad;
  Format format;
  int line_byte_count;

  Pixmap.Format pixmap_format;

  Image (Format f, Pixmap.Format pf) {
    init (0, 0, f, pf);
  }

  /**
   * Constructs an Image object and creates a new backing array.
   *
   * @param width the width in pixels
   * @param height the height in pixels
   * @param format the image format
   * @param pixmap_format the pixmap format
   */
  Image (int width, int height, Format format, Pixmap.Format pixmap_format) {
    
    init (width, height, format, pixmap_format);

    data = new byte [line_byte_count * height];
  }

  /**
   * Constructs a new Image object with an existing data array.
   *
   * @param width the width in pixels
   * @param height the height in pixels
   * @param format the image format
   * @param pixmap_format the pixmap format
   * @param data the underlying data array
   */
  Image (int width, int height, Format format, Pixmap.Format pixmap_format,
         byte[] data) {
    init (width, height, format, pixmap_format);
    this.data = data;
  }

  /**
   * Initializes the internal state of the Image object.
   *
   * @param w the width
   * @param h the height
   * @param f the image format
   * @param pf the pixmap format
   */
  private void init (int w, int h, Format f, Pixmap.Format pf) {

    width = w;
    height = h;
    format = f;
    pixmap_format = pf;

    init ();
  }

  /**
   * Calculates the internal state.
   */
  void init () {

    // compute line_byte_count
    int line_bit_count = width * pixmap_format.bits_per_pixel ();
    int rem = line_bit_count % pixmap_format.scanline_pad ();
    int line_pad_count = line_bit_count / pixmap_format.scanline_pad ()
      + (rem == 0 ? 0 : 1);
    line_byte_count = line_pad_count * pixmap_format.scanline_pad () / 8;
    left_pad = format == Format.ZPIXMAP ? 0 
                                        : line_byte_count * 8 - line_bit_count;
  }

  /**
   * Returns the width of the image in pixels.
   *
   * @return the width of the image in pixels
   */
  public int get_width () {
    return width;
  }

  /**
   * Returns the height of the image in pixels.
   *
   * @return the height of the image in pixels
   */
  public int get_height () {
    return height;
  }

  /**
   * Returns the image format.
   *
   * @return the image format
   */
  public Format get_format () {
    return format;
  }

  public byte[] get_data () {
    return data;
  }

  public int get_left_pad () {
    return left_pad;
  }

  public Pixmap.Format get_pixmap_format () {
    return pixmap_format;
  }

  public int get_line_byte_count () {
    return line_byte_count;
  }
}
