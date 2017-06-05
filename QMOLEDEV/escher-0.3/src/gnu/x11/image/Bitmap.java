package gnu.x11.image;

import gnu.x11.Display;


public class Bitmap extends Image {
  public static final int FORMAT = 0;
  public int unit_byte_count, scanline_unit;


  public Bitmap (Display display, int width, int height) {
    super (width, height, Format.BITMAP, display.pixmap_formats [0]);

    if (display.image_byte_order != LSB_FIRST
      || display.bitmap_format_bit_order != LEAST_SIGNIFICANT)
      throw new Error ("Unsupported image format");
    
    scanline_unit = display.bitmap_format_scanline_unit;
    unit_byte_count = scanline_unit / 8;
  }


  public void set (int x, int y) {
    // only support my system for now
    // 4Ll	07-00 15-08 23-16 31-24

    // bitmap unit, bit order, padding:    32, LSBFirst, 32
    // image byte order:    LSBFirst

    int xx = left_pad + x;	// shifted x
    int unit_index = xx / scanline_unit;
    int unit_byte_index = (xx % scanline_unit) / 8; // LSBFirst
    int bit_index = xx % 8; // LeastSignificant
    int index = y * line_byte_count 
      + unit_index * unit_byte_count
      + unit_byte_index;
    
    data [index] |= 1<<bit_index;
  }
}
