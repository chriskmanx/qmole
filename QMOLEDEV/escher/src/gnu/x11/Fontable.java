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
    private int minCharOrByte2;
    private int maxCharOrByte2;
    private int defaultChar;
    private int drawDirection;
    private int minByte1;
    private int maxByte1;
    private boolean allCharsExist;
    private int fontAscent;
    private int fontDescent;
    private FontProperty [] properties;
    private CharInfo [] charInfos;

    FontInfo (ResponseInputStream i) {
      min_bounds = new CharInfo (i);
      i.skip (4);
      max_bounds = new CharInfo (i);
      i.skip (4);
      minCharOrByte2 = i.readInt16 ();
      maxCharOrByte2 = i.readInt16 ();
      defaultChar = i.readInt16 ();
      int num_props = i.readInt16 ();
      drawDirection = i.readInt8 ();
      minByte1 = i.readInt8 ();
      maxByte1 = i.readInt8 ();
      allCharsExist = i.readBool ();
      fontAscent = i.readInt16 ();
      fontDescent = i.readInt16 ();
      int num_charinfo = i.readInt32 ();
      properties = new FontProperty [num_props];
      for (int j = 0; j < num_props; j++)
        properties [j] = new FontProperty (i);
      charInfos = new CharInfo [num_charinfo];
      for (int j = 0; j < num_charinfo; j++)
        charInfos [j] = new CharInfo (i);
    }

    /**
     * Encapsulate an additional font property.
     */
    public class FontProperty {

      private int name_id;
      private int value;
      
      /**
       * Creates a new FontProperty that starts at the specified index.
       *
       * @param i the starting index of the font property
       */
      private FontProperty (ResponseInputStream i) {
        name_id = i.readInt32 ();
        value = i.readInt32 ();
      }

      public Atom name () {
        return (Atom) Atom.intern (display, name_id);
      }
    }

    /**
     * Encapsulates information about one character.
     */
    public class CharInfo {

      private int leftSideBearing;
      private int rightSideBearing;
      private int characterWidth;
      private int ascent;
      private int descent;
      private int attributes;

      /**
       * Creates a new CharInfo instance that starts at the specified offset
       * in the response.
       *
       * @param i the starting index of the CharInfo field
       */
      private CharInfo (ResponseInputStream i) {
        leftSideBearing = i.readInt16 ();
        rightSideBearing = i.readInt16 ();
        characterWidth = i.readInt16 ();
        ascent = i.readInt16 ();
        descent = i.readInt16 ();
        attributes = i.readInt16 ();
      }

      public int character_width () {
        return characterWidth;
      }
    }

    public static final int LEFT_TO_RIGHT = 0;
    public static final int RIGHT_TO_LEFT = 1;

    public int font_ascent () {
      return fontAscent;
    }

    public int font_descent () {
      return fontDescent;
    }

    public CharInfo max_bounds () {
      return max_bounds;
    }

    public CharInfo min_bounds () {
      return min_bounds;
    }

    public int max_byte1 () {
      return maxByte1;
    }

    public int min_byte1 () {
      return minByte1;
    }

    public int min_char_or_byte2 () {
      return minCharOrByte2;
    }

    public int max_char_or_byte2 () {
      return maxCharOrByte2;
    }

    public CharInfo[] char_infos () {
      return charInfos;
    }
  }
  
  
  // opcode 47 - query font  
  /**
   * @see <a href="XQueryFont.html">XQueryFont</a>
   */
  public FontInfo info () {

    FontInfo info;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (47, 0, 2);
      o.writeInt32 (id);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized (i) {
        i.readReply (o);
        i.skip (8);
        info = new FontInfo (i);
      }
    }

    return info;
  }

  
  /** Reply of {@link #textExtent(String)}. */
  public class TextExtentInfo {

    private boolean leftToRight;
    private int fontAscent;
    private int fontDescent;
    private int overallAscent;
    private int overallDescent;
    private int overallWidth;
    private int overallLeft;
    private int overallRight;

    TextExtentInfo (ResponseInputStream i) {
      leftToRight = i.readBool ();
      i.skip (6);
      fontAscent = i.readInt16 ();
      fontDescent = i.readInt16 ();
      overallAscent = i.readInt16 ();
      overallDescent = i.readInt16 ();
      overallWidth = i.readInt32 ();
      overallLeft = i.readInt32 ();
      overallRight = i.readInt32 ();
    }

    public int overallWidth () {
      return overallWidth;
    }
  }

  // opcode 48 - query text extents  
  /**
   * @see <a href="XQueryTextExtents.html">XQueryTextExtents</a>
   */
  public TextExtentInfo textExtent (String s) {

    TextExtentInfo info;
    boolean odd = s.length () % 2 == 1;
    int pad = odd ? 2 : 0;
    int len = 2 + (2 * s.length () + pad) / 4;

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (48, odd ? 1 : 0, len);
      o.writeInt32 (id);
      o.writeString16 (s);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized (i) {
        i.readReply (o);
        i.skip (1);
        info = new TextExtentInfo (i);
        i.skip (4);
      }
    }
    return info;
  }
}
