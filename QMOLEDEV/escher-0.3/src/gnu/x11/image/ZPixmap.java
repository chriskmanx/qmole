package gnu.x11.image;

import gnu.x11.Color;
import gnu.x11.Display;
import gnu.x11.Pixmap;


public class ZPixmap extends Image {

  int image_byte_order;
  int pixel_byte_count;

  ZPixmap (Display display) {        // for subclass loading
    super(Format.ZPIXMAP, display.default_pixmap_format);
    image_byte_order = display.image_byte_order;
    pixel_byte_count = pixmap_format.bits_per_pixel () / 8;

    if (display.default_depth < 24)
      throw new Error ("Unsupported root depth < 24: " +
        display.default_depth);
  }

  /**
   * Creates a new ZPixmap with the specified width and height and with
   * the default pixmap format from the display.
   *
   * @param display the display on which to create the pixmap
   * @param width the width in pixels
   * @param height the height in pixels
   */
  public ZPixmap (Display display, int width, int height)
  {
    this (display, width, height, display.default_pixmap_format);
  }

  public ZPixmap (Display display, int width, int height,
                  Pixmap.Format format) {

    super (width, height, Format.ZPIXMAP, format);

    image_byte_order = display.image_byte_order;
    pixel_byte_count = pixmap_format.bits_per_pixel () / 8;

    if (display.default_depth < 24)
      throw new Error ("Unsupported root depth < 24: " +
        display.default_depth);
  }

  /**
   * Creates a ZPixmap with the specified size and format, and initializes
   * the underlying data array with an existing one.
   *
   * @param display the
   * @param width
   * @param height
   * @param format
   * @param data
   */
  public ZPixmap (Display display, int width, int height, Pixmap.Format format,
                  byte[] data) {
    this (display, width, height, format);
    this.data = data; 
  }

  public void set (int x, int y, Color color) {
    set (x, y, color.pixel);
  }


  public void set (int x, int y, int pixel) {
    int i = y * line_byte_count + pixel_byte_count * x;

    // outside for loop for speed
    if (image_byte_order == LSB_FIRST)
      for (int j=0; j<pixel_byte_count; j++)
	data [i+j] = (byte) (0xff & (pixel >> j*8));

    else			// MSB_FIRST
      for (int j=0; j<pixel_byte_count; j++)
	data [i+j] = (byte) (0xff & (pixel >> (pixel_byte_count-1-j)*8));
  }


  public void set_red (int x, int y, int r) {
    int i = y * line_byte_count + pixel_byte_count * x;
    if (image_byte_order == LSB_FIRST) i += 2;
    data [i] = (byte) r;
  }

  public int get_red (int x, int y) {
    int i = y * line_byte_count + pixel_byte_count * x;
    if (image_byte_order == LSB_FIRST) i += 2;
    return 0xff & data [i];
  }

  public void set_green (int x, int y, int g) {
    int i = y * line_byte_count + pixel_byte_count * x + 1;
    data [i] = (byte) g;
  }

  public int get_green (int x, int y) {
    int i = y * line_byte_count + pixel_byte_count * x + 1;
    return 0xff & data [i];
  }

  public void set_blue (int x, int y, int b) {
    int i = y * line_byte_count + pixel_byte_count * x;
    if (image_byte_order == MSB_FIRST) i += 2;
    data [i] = (byte) b;
  }

  public int get_blue (int x, int y) {
    int i = y * line_byte_count + pixel_byte_count * x;
    if (image_byte_order == MSB_FIRST) i += 2;
    return 0xff & data [i];
  }

  public void set (int x, int y, int r, int g, int b) {
    int i = y * line_byte_count + pixel_byte_count * x;

    // outside for loop for speed
    if (image_byte_order == LSB_FIRST) {
      data [i] = (byte) b;
      data [i+1] = (byte) g;
      data [i+2] = (byte) r;

    } else {			// MSB_FIRST
      data [i] = (byte) r;
      data [i+1] = (byte) g;
      data [i+2] = (byte) b;
    }
  }

  /**
   * Puts the image data into this image. This data must be in the same format
   * as specified in this image.
   *
   * @param image_data the data to set
   */
  public void set_data (int[] image_data) {
    int len = pixel_byte_count * width * height;
    len = Math.min(len, data.length);
    System.arraycopy(image_data, 0, data, 0, len);
  }

  /**
   * Sets a data element in the ZPixmap directly.
   *
   * This manipulates the underlying data directly. It is recommended
   * to use one of the other accessor methods instead.
   *
   * @param index the index of the data element to set
   * @param val the value
   */  
  public void set_data_element (int index, byte val) {
    data[index] = val;
  }

  /**
   * Returns the data element at the specified index.
   *
   * This manipulates the underlying data directly. It is recommended
   * to use one of the other accessor methods instead.
   *
   * @param index the index of the data element
   *
   * @return the data element at the specified index
   */
  public byte get_data_element (int index) {
    return data[index];
  }

  /**
   * Returns the length of the underlying data array.
   *
   * @return the length of the underlying data array
   */
  public int get_data_length () {
    return data.length;
  }
}
