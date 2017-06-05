package gnu.x11;


/** X visual. */
public class Visual {

  public static final Visual COPY_FROM_PARENT = new Visual ();

  public int id;
  public int visual_class;
  public int bits_per_rgb_value;
  public int colormap_entries;
  public int red_mask;
  public int green_mask;
  public int blue_mask;

  private Visual () {}

  Visual (ResponseInputStream in) {
    id = in.read_int32 ();
    visual_class = in.read_int8 ();
    bits_per_rgb_value = in.read_int8 ();
    colormap_entries = in.read_int16();
    red_mask = in.read_int32 ();
    green_mask = in.read_int32 ();
    blue_mask = in.read_int32 ();
    in.skip (4);

  }
  public static final int STATIC_GRAY = 0;
  public static final int GRAY_SCALE = 1;
  public static final int STATIC_COLOR = 2;
  public static final int PSEUDO_COLOR = 3;
  public static final int TRUE_COLOR = 4;
  public static final int DIRECT_COLOR = 5;


  public static final String [] CLASS_STRINGS = {
    "static-gray", "gray-scale", "static-color", "pseudo-color", 
    "true-color", "direct-color" 
  };

  public String toString () {
    return "#Visual"
      + "\n  id: " + id
      + "\n  class: " + CLASS_STRINGS [visual_class]
      + "\n  bits-per-rgb-value: " + bits_per_rgb_value
      + "\n  colormap-entries: " + colormap_entries
      + "\n  red-mask: 0x" + Integer.toHexString (red_mask)
      + "\n  green-mask: 0x " + Integer.toHexString (green_mask)
      + "\n  blue-mask: 0x" + Integer.toHexString (blue_mask);
  }
}
