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

  /**
   * This enum represents the shape of a Cursor.
   */
  public enum Shape {
    X_CURSOR(0), 
    ARROW(2),
    BASED_ARROW_DOWN(4),
    BASED_ARROW_UP(6),
    BOAT(8),
    BOGOSITY(10),
    BOTTOM_LEFT_CORNER(12),
    BOTTOM_RIGHT_CORNER(14),
    BOTTOM_SIDE(16),
    BOTTOM_TEE(18),
    BOX_SPIRAL(20),
    CENTER_PTR(22),
    CIRCLE(24),
    CLOCK(26),
    COFFEE_MUG(28),
    CROSS(30),
    CROSS_REVERSE(32),
    CROSSHAIR(34),
    DIAMOND_CROSS(36),
    DOT(38),
    DOTBOX(40),
    DOUBLE_ARROW(42),
    DRAFT_LARGE(44),
    DRAFT_SMALL(46),
    DRAPED_BOX(48),
    EXCHANGE(50),
    FLEUR(52),
    GOBBLER(54),
    GUMBY(56),
    HAND1(58),
    HAND2(60),
    HEART(62),
    ICON(64),
    IRON_CROSS(66),
    LEFT_PTR(68),
    LEFT_SIDE(70),
    LEFT_TEE(72),
    LEFTBUTTON(74),
    LL_ANGLE(76),
    LR_ANGLE(78),
    MAN(80),
    MIDDLEBUTTON(82),
    MOUSE(84),
    PENCIL(86),
    PIRATE(88),
    PLUS(90),
    QUESTION_ARROW(92),
    RIGHT_PTR(94),
    RIGHT_SIDE(96),
    RIGHT_TEE(98),
    RIGHTBUTTON(100),
    RTL_LOGO(102),
    SAILBOAT(104),
    SB_DOWN_ARROW(106),
    SB_H_DOUBLE_ARROW(108),
    SB_LEFT_ARROW(110),
    SB_RIGHT_ARROW(112),
    SB_UP_ARROW(114),
    SB_V_DOUBLE_ARROW(116),
    SHUTTLE(118),
    SIZING(120),
    SPIDER(122),
    SPRAYCAN(124),
    STAR(126),
    TARGET(128),
    TCROSS(130),
    TOP_LEFT_ARROW(132),
    TOP_LEFT_CORNER(134),
    TOP_RIGHT_CORNER(136),
    TOP_SIDE(138),
    TOP_TEE(140),
    TREK(142),
    UL_ANGLE(144),
    UMBRELLA(146),
    UR_ANGLE(148),
    WATCH(150),
    XTERM(152);
    
    private int cursorID;

    private Shape(int id) {
      this.cursorID = id;
    }
    
    public int getShapeID() {
      return this.cursorID;
    }
    
    public String getShapeName() {
      return this.name();
    }
  }
  
  // cursor font
  public static Font FONT = null;

  /** Predefined. */
  public Cursor (int id) {
    super (id);
  }

  /**
   * @see <a href="XCreateFontCursor.html">XCreateFontCursor</a>
   */  
  public Cursor (Display display, Shape shape) {
    super (display);
    
    // X predefined special font
    if (FONT == null) FONT = new Font (display, "cursor");

    /* From <X11/Cursor.c>: The cursor font contains the shape glyph
     * followed by the mask glyph; so character position 0 contains a
     * shape, 1 the mask for 0, 2 a shape, etc.
     */

    // black and white
    create (FONT, FONT, shape.getShapeID(), shape.getShapeID() + 1, 0, 0, 0, 1, 1, 1);
  }


  /**
   * @see <a href="XCreateGlyphCursor.html">XCreateGlyphCursor</a>
   */
  public Cursor (Font src, Font mask, Shape source_char,  
    Shape mask_char, int fg_r, int fg_g, int fg_b, 
    int bg_r, int bg_g, int bg_b) { 

    super (src.display);
    create (src, mask, source_char.getShapeID(), mask_char.getShapeID(), fg_r, fg_g, fg_b,
      bg_r, bg_g, bg_b);
  }


  // opcode 93 - create cursor
  //@Question The variables mask_char and source_char aren't used here, do we need them?
  //@Question Or they are here to make 'compatible' with the X protocol?
  /**
   * @param mask possible: {@link Pixmap#NONE}
   * @see <a href="XCreatePixmapCursor.html">XCreatePixmapCursor</a>
   */
  public Cursor (Pixmap src, Pixmap mask, Shape source_char,  
                 Shape mask_char, int fg_r, int fg_g, int fg_b, 
                 int bg_r, int bg_g, int bg_b, int x, int y) { 
    super (src.display);

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (93, 0, 8);
      o.writeInt32 (id);
      o.writeInt32 (src.id);
      o.writeInt32 (mask.id);
      o.writeInt16 (fg_r);
      o.writeInt16 (fg_g);
      o.writeInt16 (fg_b);
      o.writeInt16 (bg_r);
      o.writeInt16 (bg_g);
      o.writeInt16 (bg_b);
      o.writeInt16 (x);
      o.writeInt16 (y);
      o.send ();
    }
  }


  // opcode 94 - create glyph cursor
  public void create (Font src, Font mask, int source_char, 
                      int mask_char, int fg_r, int fg_g, int fg_b, 
                      int bg_r, int bg_g, int bg_b) {

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (94, 0, 8);
      o.writeInt32 (id);
      o.writeInt32 (src.id);
      o.writeInt32 (mask.id);
      o.writeInt16 (source_char);
      o.writeInt16 (mask_char);
      o.writeInt16 (fg_r);
      o.writeInt16 (fg_g);
      o.writeInt16 (fg_b);
      o.writeInt16 (bg_r);
      o.writeInt16 (bg_g);
      o.writeInt16 (bg_b);
      o.send ();
      
    }
  }


  // opcode 95 - free cursor
  /**
   * @see <a href="XFreeCursor.html">XFreeCursor</a>
   */
  public void free () {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (95, 0, 2);
      o.writeInt32 (id);
      o.send ();
    }
  }


  // opcode 96 - recolor cursor
  /**
   * @see <a href="XRecolorCursor.html">XRecolorCursor</a>
   */
  public void recolor (RGB foreground, RGB background) {

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (96, 0, 5);
      o.writeInt32 (id);
      o.writeInt16 (foreground.getRed());
      o.writeInt16 (foreground.getGreen());
      o.writeInt16 (foreground.getBlue());
      o.writeInt16 (background.getRed());
      o.writeInt16 (background.getRed());
      o.writeInt16 (background.getBlue());
      o.send ();
    }
  }
}