package gnu.x11;


/** X fontable. */
public abstract class Fontable extends Resource {
  /** Predefined. */
  public Fontable (int id) {
    super (id);
  }


  /** Create. */
  public Fontable (Display display) {
    super (display);
  }


  /** Intern. */
  public Fontable (Display display, int id) {
    super (display, id);
  }


  /** Reply of {@link #info()}. */
  public class FontInfo {

    private CharInfo min_bounds;
    private CharInfo max_bounds;
    private int min_char_or_byte2;
    private int max_char_or_byte2;
    public int default_char;
    public int draw_direction;
    private int min_byte1;
    private int max_byte1;
    public boolean all_chars_exist;
    private int font_ascent;
    private int font_descent;
    public FontProperty [] properties;
    private CharInfo [] char_infos;

    FontInfo (ResponseInputStream i) {
      min_bounds = new CharInfo (i);
      i.skip (4);
      max_bounds = new CharInfo (i);
      i.skip (4);
      min_char_or_byte2 = i.read_int16 ();
      max_char_or_byte2 = i.read_int16 ();
      default_char = i.read_int16 ();
      int num_props = i.read_int16 ();
      draw_direction = i.read_int8 ();
      min_byte1 = i.read_int8 ();
      max_byte1 = i.read_int8 ();
      all_chars_exist = i.read_bool ();
      font_ascent = i.read_int16 ();
      font_descent = i.read_int16 ();
      int num_charinfo = i.read_int32 ();
      properties = new FontProperty [num_props];
      for (int j = 0; j < num_props; j++)
        properties [j] = new FontProperty (i);
      char_infos = new CharInfo [num_charinfo];
      for (int j = 0; j < num_charinfo; j++)
        char_infos [j] = new CharInfo (i);
    }

    /**
     * Encapsulate an additional font property.
     */
    public class FontProperty {

      public int name_id;
      public int value;
      
      /**
       * Creates a new FontProperty that starts at the specified index.
       *
       * @param i the starting index of the font property
       */
      private FontProperty (ResponseInputStream i) {
        name_id = i.read_int32 ();
        value = i.read_int32 ();
      }

      public Atom name () {
        return (Atom) Atom.intern (display, name_id);
      }
    }

    /**
     * Encapsulates information about one character.
     */
    public class CharInfo {

      public int left_side_bearing;
      public int right_side_bearing;
      public int character_width;
      public int ascent;
      public int descent;
      public int attributes;

      /**
       * Creates a new CharInfo instance that starts at the specified offset
       * in the response.
       *
       * @param i the starting index of the CharInfo field
       */
      private CharInfo (ResponseInputStream i) {
        left_side_bearing = i.read_int16 ();
        right_side_bearing = i.read_int16 ();
        character_width = i.read_int16 ();
        ascent = i.read_int16 ();
        descent = i.read_int16 ();
        attributes = i.read_int16 ();
      }

      public int character_width () {
        return character_width;
      }
    }

    public static final int LEFT_TO_RIGHT = 0;
    public static final int RIGHT_TO_LEFT = 1;

    public int font_ascent () {
      return font_ascent;
    }

    public int font_descent () {
      return font_descent;
    }

    public CharInfo max_bounds () {
      return max_bounds;
    }

    public CharInfo min_bounds () {
      return min_bounds;
    }

    public int max_byte1 () {
      return max_byte1;
    }

    public int min_byte1 () {
      return min_byte1;
    }

    public int min_char_or_byte2 () {
      return min_char_or_byte2;
    }

    public int max_char_or_byte2 () {
      return max_char_or_byte2;
    }

    public CharInfo[] char_infos () {
      return char_infos;
    }
  }
  
  
  // opcode 47 - query font  
  /**
   * @see <a href="XQueryFont.html">XQueryFont</a>
   */
  public FontInfo info () {

    FontInfo info;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (47, 0, 2);
      o.write_int32 (id);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        info = new FontInfo (i);
      }
    }

    return info;
  }

  
  /** Reply of {@link #text_extent(String)}. */
  public class TextExtentInfo {

    public boolean left_to_right;
    public int font_ascent;
    public int font_descent;
    public int overall_ascent;
    public int overall_descent;
    private int overall_width;
    public int overall_left;
    public int overall_right;

    TextExtentInfo (ResponseInputStream i) {
      left_to_right = i.read_bool ();
      i.skip (6);
      font_ascent = i.read_int16 ();
      font_descent = i.read_int16 ();
      overall_ascent = i.read_int16 ();
      overall_descent = i.read_int16 ();
      overall_width = i.read_int32 ();
      overall_left = i.read_int32 ();
      overall_right = i.read_int32 ();
    }

    public int overall_width () {
      return overall_width;
    }
  }

  // opcode 48 - query text extents  
  /**
   * @see <a href="XQueryTextExtents.html">XQueryTextExtents</a>
   */
  public TextExtentInfo text_extent (String s) {

    TextExtentInfo info;
    boolean odd = s.length () % 2 == 1;
    int pad = odd ? 2 : 0;
    int len = 2 + (2 * s.length () + pad) / 4;

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (48, odd ? 1 : 0, len);
      o.write_int32 (id);
      o.write_string16 (s);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        info = new TextExtentInfo (i);
        i.skip (4);
      }
    }
    return info;
  }
}
