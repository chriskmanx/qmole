package gnu.x11.image;

import gnu.x11.Color;
import gnu.x11.Colormap;
import gnu.x11.Display;
import java.util.StringTokenizer;


public class XPM extends ZPixmap { // TODO
  public XPM (Display display, String [] xpm) {
    super (display);

    StringTokenizer values = new StringTokenizer (xpm [0]);
    width = Integer.parseInt (values.nextToken ());
    height = Integer.parseInt (values.nextToken ());
    int num_colors = Integer.parseInt (values.nextToken ());
    int chars_per_pixel = Integer.parseInt (values.nextToken ());

    // TODO how to select best SUITABLE format?
    format = Format.ZPIXMAP;
    pixmap_format = display.default_pixmap_format;
    image_byte_order = display.image_byte_order;
    pixel_byte_count = pixmap_format.bits_per_pixel () / 8;
    init ();

    java.util.Hashtable mapping = new java.util.Hashtable (2*num_colors);
    Colormap cmap = display.default_colormap;
    
    for (int i=0; i<num_colors; i++) {
      StringTokenizer color = new StringTokenizer (xpm [i+1]);
      String symbol = color.nextToken ();
      
      while (color.hasMoreTokens () 
	&& color.nextToken ().equals ("c")) { // can only parse c for now
	
	String color_value = color.nextToken ();

	if (color_value.charAt (0) == '#') {
	  int pixel = Integer.parseInt (
	    color_value.substring (1, color_value.length ()), 16);

	  mapping.put (symbol, new Color (pixel));

	} else			// not starting with #
	  mapping.put (symbol, cmap.alloc_named_color (color_value));
      }
    }

    // TODO more efficient linear for-loop
    for (int y=0; y<height; y++) {
      String pixels = xpm [1+num_colors+y];

      for (int x=0; x<width; x++) {
	String symbol = pixels.substring (x*chars_per_pixel,
	  (x+1)*chars_per_pixel);
	Color color = (Color) mapping.get (symbol);
	set (x, y, color.pixel);
      }
    }
  }
}


