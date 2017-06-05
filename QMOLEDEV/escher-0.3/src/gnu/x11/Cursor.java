package gnu.x11;


/** X cursor. */
public class Cursor extends Resource {
  /** 
   * Predefined cursor.
   *
   * @see Window#NONE
   */
  public static final Cursor NONE = new Cursor (0);
  public static final Cursor CURRENT = new Cursor (1);


  // cursor shapes
  public static final int X_CURSOR = 0;
  public static final int ARROW = 2;
  public static final int BASED_ARROW_DOWN = 4;
  public static final int BASED_ARROW_UP = 6;
  public static final int BOAT = 8;
  public static final int BOGOSITY = 10;
  public static final int BOTTOM_LEFT_CORNER = 12;
  public static final int BOTTOM_RIGHT_CORNER = 14;
  public static final int BOTTOM_SIDE = 16;
  public static final int BOTTOM_TEE = 18;
  public static final int BOX_SPIRAL = 20;
  public static final int CENTER_PTR = 22;
  public static final int CIRCLE = 24;
  public static final int CLOCK = 26;
  public static final int COFFEE_MUG = 28;
  public static final int CROSS = 30;
  public static final int CROSS_REVERSE = 32;
  public static final int CROSSHAIR = 34;
  public static final int DIAMOND_CROSS = 36;
  public static final int DOT = 38;
  public static final int DOTBOX = 40;
  public static final int DOUBLE_ARROW = 42;
  public static final int DRAFT_LARGE = 44;
  public static final int DRAFT_SMALL = 46;
  public static final int DRAPED_BOX = 48;
  public static final int EXCHANGE = 50;
  public static final int FLEUR = 52;
  public static final int GOBBLER = 54;
  public static final int GUMBY = 56;
  public static final int HAND1 = 58;
  public static final int HAND2 = 60;
  public static final int HEART = 62;
  public static final int ICON = 64;
  public static final int IRON_CROSS = 66;
  public static final int LEFT_PTR = 68;
  public static final int LEFT_SIDE = 70;
  public static final int LEFT_TEE = 72;
  public static final int LEFTBUTTON = 74;
  public static final int LL_ANGLE = 76;
  public static final int LR_ANGLE = 78;
  public static final int MAN = 80;
  public static final int MIDDLEBUTTON = 82;
  public static final int MOUSE = 84;
  public static final int PENCIL = 86;
  public static final int PIRATE = 88;
  public static final int PLUS = 90;
  public static final int QUESTION_ARROW = 92;
  public static final int RIGHT_PTR = 94;
  public static final int RIGHT_SIDE = 96;
  public static final int RIGHT_TEE = 98;
  public static final int RIGHTBUTTON = 100;
  public static final int RTL_LOGO = 102;
  public static final int SAILBOAT = 104;
  public static final int SB_DOWN_ARROW = 106;
  public static final int SB_H_DOUBLE_ARROW = 108;
  public static final int SB_LEFT_ARROW = 110;
  public static final int SB_RIGHT_ARROW = 112;
  public static final int SB_UP_ARROW = 114;
  public static final int SB_V_DOUBLE_ARROW = 116;
  public static final int SHUTTLE = 118;
  public static final int SIZING = 120;
  public static final int SPIDER = 122;
  public static final int SPRAYCAN = 124;
  public static final int STAR = 126;
  public static final int TARGET = 128;
  public static final int TCROSS = 130;
  public static final int TOP_LEFT_ARROW = 132;
  public static final int TOP_LEFT_CORNER = 134;
  public static final int TOP_RIGHT_CORNER = 136;
  public static final int TOP_SIDE = 138;
  public static final int TOP_TEE = 140;
  public static final int TREK = 142;
  public static final int UL_ANGLE = 144;
  public static final int UMBRELLA = 146;
  public static final int UR_ANGLE = 148;
  public static final int WATCH = 150;
  public static final int XTERM = 152;


  // cursor font
  public static Font FONT = null;
  

  /** Predefined. */
  public Cursor (int id) {
    super (id);
  }


  /**
   * @see <a href="XCreateFontCursor.html">XCreateFontCursor</a>
   */  
  public Cursor (Display display, int shape) {
    super (display);
    
    // X predefined special font
    if (FONT == null) FONT = new Font (display, "cursor");

    /* From <X11/Cursor.c>: The cursor font contains the shape glyph
     * followed by the mask glyph; so character position 0 contains a
     * shape, 1 the mask for 0, 2 a shape, etc.
     */

    // black and white
    create (FONT, FONT, shape, shape+1, 0, 0, 0, 1, 1, 1);
  }


  /**
   * @see <a href="XCreateGlyphCursor.html">XCreateGlyphCursor</a>
   */
  public Cursor (Font src, Font mask, int source_char,  
    int mask_char, int fg_r, int fg_g, int fg_b, 
    int bg_r, int bg_g, int bg_b) { 

    super (src.display);
    create (src, mask, source_char, mask_char, fg_r, fg_g, fg_b,
      bg_r, bg_g, bg_b);
  }


  // opcode 93 - create cursor
  /**
   * @param mask possible: {@link Pixmap#NONE}
   * @see <a href="XCreatePixmapCursor.html">XCreatePixmapCursor</a>
   */
  public Cursor (Pixmap src, Pixmap mask, int source_char,  
                 int mask_char, int fg_r, int fg_g, int fg_b, 
                 int bg_r, int bg_g, int bg_b, int x, int y) { 

    super (src.display);

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (93, 0, 8);
      o.write_int32 (id);
      o.write_int32 (src.id);
      o.write_int32 (mask.id);
      o.write_int16 (fg_r);
      o.write_int16 (fg_g);
      o.write_int16 (fg_b);
      o.write_int16 (bg_r);
      o.write_int16 (bg_g);
      o.write_int16 (bg_b);
      o.write_int16 (x);
      o.write_int16 (y);
      o.send ();
    }
  }


  // opcode 94 - create glyph cursor
  public void create (Font src, Font mask, int source_char, 
                      int mask_char, int fg_r, int fg_g, int fg_b, 
                      int bg_r, int bg_g, int bg_b) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (94, 0, 8);
      o.write_int32 (id);
      o.write_int32 (src.id);
      o.write_int32 (mask.id);
      o.write_int16 (source_char);
      o.write_int16 (mask_char);
      o.write_int16 (fg_r);
      o.write_int16 (fg_g);
      o.write_int16 (fg_b);
      o.write_int16 (bg_r);
      o.write_int16 (bg_g);
      o.write_int16 (bg_b);
      o.send ();
      
    }
  }


  // opcode 95 - free cursor
  /**
   * @see <a href="XFreeCursor.html">XFreeCursor</a>
   */
  public void free () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (95, 0, 2);
      o.write_int32 (id);
      o.send ();
    }
  }


  // opcode 96 - recolor cursor
  /**
   * @see <a href="XRecolorCursor.html">XRecolorCursor</a>
   */
  public void recolor (RGB foreground, RGB background) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (96, 0, 5);
      o.write_int32 (id);
      o.write_int16 (foreground.red);
      o.write_int16 (foreground.green);
      o.write_int16 (foreground.blue);
      o.write_int16 (background.red);
      o.write_int16 (background.green);
      o.write_int16 (background.blue);
      o.send ();
    }
  }
}
