package gnu.x11;


/** X atom. */
public class Atom {
  // predefined atom ids
  public static final int ANY_PROPERTY_TYPE_ID = 0;
  public static final int PRIMARY_ID = 1;
  public static final int SECONDARY_ID = 2;
  public static final int ARC_ID = 3;
  public static final int ATOM_ID = 4;
  public static final int BITMAP_ID = 5;
  public static final int CARDINAL_ID = 6;
  public static final int COLORMAP_ID = 7;
  public static final int CURSOR_ID = 8;
  public static final int CUT_BUFFER0_ID = 9;
  public static final int CUT_BUFFER1_ID = 10;
  public static final int CUT_BUFFER2_ID = 11;
  public static final int CUT_BUFFER3_ID = 12;
  public static final int CUT_BUFFER4_ID = 13;
  public static final int CUT_BUFFER5_ID = 14;
  public static final int CUT_BUFFER6_ID = 15;
  public static final int CUT_BUFFER7_ID = 16;
  public static final int DRAWABLE_ID = 17;
  public static final int FONT_ID = 18;
  public static final int INTEGER_ID = 19;
  public static final int PIXMAP_ID = 20;
  public static final int POINT_ID = 21;
  public static final int RECTANGLE_ID = 22;
  public static final int RESOURCE_MANAGER_ID = 23;
  public static final int RGB_COLOR_MAP_ID = 24;
  public static final int RGB_BEST_MAP_ID = 25;
  public static final int RGB_BLUE_MAP_ID = 26;
  public static final int RGB_DEFAULT_MAP_ID = 27;
  public static final int RGB_GRAY_MAP_ID = 28;
  public static final int RGB_GREEN_MAP_ID = 29;
  public static final int RGB_RED_MAP_ID = 30;
  public static final int STRING_ID = 31;
  public static final int VISUALID_ID = 32;
  public static final int WINDOW_ID = 33;
  public static final int WM_COMMAND_ID = 34;
  public static final int WM_HINTS_ID = 35;
  public static final int WM_CLIENT_MACHINE_ID = 36;
  public static final int WM_ICON_NAME_ID = 37;
  public static final int WM_ICON_SIZE_ID = 38;
  public static final int WM_NAME_ID = 39;
  public static final int WM_NORMAL_HINTS_ID = 40;
  public static final int WM_SIZE_HINTS_ID = 41;
  public static final int WM_ZOOM_HINTS_ID = 42;
  public static final int MIN_SPACE_ID = 43;
  public static final int NORM_SPACE_ID = 44;
  public static final int MAX_SPACE_ID = 45;
  public static final int END_SPACE_ID = 46;
  public static final int SUPERSCRIPT_X_ID = 47;
  public static final int SUPERSCRIPT_Y_ID = 48;
  public static final int SUBSCRIPT_X_ID = 49;
  public static final int SUBSCRIPT_Y_ID = 50;
  public static final int UNDERLINE_POSITION_ID = 51;
  public static final int UNDERLINE_THICKNESS_ID = 52;
  public static final int STRIKEOUT_ASCENT_ID = 53;
  public static final int STRIKEOUT_DESCENT_ID = 54;
  public static final int ITALIC_ANGLE_ID = 55;
  public static final int X_HEIGHT_ID = 56;
  public static final int QUAD_WIDTH_ID = 57;
  public static final int WEIGHT_ID = 58;
  public static final int POINT_SIZE_ID = 59;
  public static final int RESOLUTION_ID = 60;
  public static final int COPYRIGHT_ID = 61;
  public static final int NOTICE_ID = 62;
  public static final int FONT_NAME_ID = 63;
  public static final int FAMILY_NAME_ID = 64;
  public static final int FULL_NAME_ID = 65;
  public static final int CAP_HEIGHT_ID = 66;
  public static final int WM_CLASS_ID = 67;
  public static final int WM_TRANSIENT_FOR_ID = 68;


  /**
   * Predefined atoms. 
   *
   * @see Window#NONE
   */
  public static final Atom NONE
    = new Atom (0, "NONE");
  public static final Atom ANY_PROPERTY_TYPE 
    = new Atom (ANY_PROPERTY_TYPE_ID, "ANY_PROPERTY_TYPE");
  public static final Atom PRIMARY 
    = new Atom (PRIMARY_ID, "PRIMARY");
  public static final Atom SECONDARY 
    = new Atom (SECONDARY_ID, "SECONDARY");
  public static final Atom ARC 
    = new Atom (ARC_ID, "ARC");
  public static final Atom ATOM 
    = new Atom (ATOM_ID, "ATOM");
  public static final Atom BITMAP 
    = new Atom (BITMAP_ID, "BITMAP");
  public static final Atom CARDINAL 
    = new Atom (CARDINAL_ID, "CARDINAL");
  public static final Atom COLORMAP 
    = new Atom (COLORMAP_ID, "COLORMAP");
  public static final Atom CURSOR 
    = new Atom (CURSOR_ID, "CURSOR");
  public static final Atom CUT_BUFFER0 
    = new Atom (CUT_BUFFER0_ID, "CUT_BUFFER0");
  public static final Atom CUT_BUFFER1 
    = new Atom (CUT_BUFFER1_ID, "CUT_BUFFER1");
  public static final Atom CUT_BUFFER2 
    = new Atom (CUT_BUFFER2_ID, "CUT_BUFFER2");
  public static final Atom CUT_BUFFER3 
    = new Atom (CUT_BUFFER3_ID, "CUT_BUFFER3");
  public static final Atom CUT_BUFFER4 
    = new Atom (CUT_BUFFER4_ID, "CUT_BUFFER4");
  public static final Atom CUT_BUFFER5 
    = new Atom (CUT_BUFFER5_ID, "CUT_BUFFER5");
  public static final Atom CUT_BUFFER6 
    = new Atom (CUT_BUFFER6_ID, "CUT_BUFFER6");
  public static final Atom CUT_BUFFER7 
    = new Atom (CUT_BUFFER7_ID, "CUT_BUFFER7");
  public static final Atom DRAWABLE 
    = new Atom (DRAWABLE_ID, "DRAWABLE");
  public static final Atom FONT 
    = new Atom (FONT_ID, "FONT");
  public static final Atom INTEGER 
    = new Atom (INTEGER_ID, "INTEGER");
  public static final Atom PIXMAP 
    = new Atom (PIXMAP_ID, "PIXMAP");
  public static final Atom POINT 
    = new Atom (POINT_ID, "POINT");
  public static final Atom RECTANGLE 
    = new Atom (RECTANGLE_ID, "RECTANGLE");
  public static final Atom RESOURCE_MANAGER 
    = new Atom (RESOURCE_MANAGER_ID, "RESOURCE_MANAGER");
  public static final Atom RGB_COLOR_MAP 
    = new Atom (RGB_COLOR_MAP_ID, "RGB_COLOR_MAP");
  public static final Atom RGB_BEST_MAP 
    = new Atom (RGB_BEST_MAP_ID, "RGB_BEST_MAP");
  public static final Atom RGB_BLUE_MAP 
    = new Atom (RGB_BLUE_MAP_ID, "RGB_BLUE_MAP");
  public static final Atom RGB_DEFAULT_MAP 
    = new Atom (RGB_DEFAULT_MAP_ID, "RGB_DEFAULT_MAP");
  public static final Atom RGB_GRAY_MAP 
    = new Atom (RGB_GRAY_MAP_ID, "RGB_GRAY_MAP");
  public static final Atom RGB_GREEN_MAP 
    = new Atom (RGB_GREEN_MAP_ID, "RGB_GREEN_MAP");
  public static final Atom RGB_RED_MAP 
    = new Atom (RGB_RED_MAP_ID, "RGB_RED_MAP");
  public static final Atom STRING 
    = new Atom (STRING_ID, "STRING");
  public static final Atom VISUALID 
    = new Atom (VISUALID_ID, "VISUALID");
  public static final Atom WINDOW 
    = new Atom (WINDOW_ID, "WINDOW");
  public static final Atom WM_COMMAND 
    = new Atom (WM_COMMAND_ID, "WM_COMMAND");
  public static final Atom WM_HINTS 
    = new Atom (WM_HINTS_ID, "WM_HINTS");
  public static final Atom WM_CLIENT_MACHINE 
    = new Atom (WM_CLIENT_MACHINE_ID, "WM_CLIENT_MACHINE");
  public static final Atom WM_ICON_NAME 
    = new Atom (WM_ICON_NAME_ID, "WM_ICON_NAME");
  public static final Atom WM_ICON_SIZE 
    = new Atom (WM_ICON_SIZE_ID, "WM_ICON_SIZE");
  public static final Atom WM_NAME 
    = new Atom (WM_NAME_ID, "WM_NAME");
  public static final Atom WM_NORMAL_HINTS 
    = new Atom (WM_NORMAL_HINTS_ID, "WM_NORMAL_HINTS");
  public static final Atom WM_SIZE_HINTS 
    = new Atom (WM_SIZE_HINTS_ID, "WM_SIZE_HINTS");
  public static final Atom WM_ZOOM_HINTS 
    = new Atom (WM_ZOOM_HINTS_ID, "WM_ZOOM_HINTS");
  public static final Atom MIN_SPACE 
    = new Atom (MIN_SPACE_ID, "MIN_SPACE");
  public static final Atom NORM_SPACE 
    = new Atom (NORM_SPACE_ID, "NORM_SPACE");
  public static final Atom MAX_SPACE 
    = new Atom (MAX_SPACE_ID, "MAX_SPACE");
  public static final Atom END_SPACE 
    = new Atom (END_SPACE_ID, "END_SPACE");
  public static final Atom SUPERSCRIPT_X 
    = new Atom (SUPERSCRIPT_X_ID, "SUPERSCRIPT_X");
  public static final Atom SUPERSCRIPT_Y 
    = new Atom (SUPERSCRIPT_Y_ID, "SUPERSCRIPT_Y");
  public static final Atom SUBSCRIPT_X 
    = new Atom (SUBSCRIPT_X_ID, "SUBSCRIPT_X");
  public static final Atom SUBSCRIPT_Y 
    = new Atom (SUBSCRIPT_Y_ID, "SUBSCRIPT_Y");
  public static final Atom UNDERLINE_POSITION 
    = new Atom (UNDERLINE_POSITION_ID, "UNDERLINE_POSITION");
  public static final Atom UNDERLINE_THICKNESS 
    = new Atom (UNDERLINE_THICKNESS_ID, "UNDERLINE_THICKNESS");
  public static final Atom STRIKEOUT_ASCENT 
    = new Atom (STRIKEOUT_ASCENT_ID, "STRIKEOUT_ASCENT");
  public static final Atom STRIKEOUT_DESCENT 
    = new Atom (STRIKEOUT_DESCENT_ID, "STRIKEOUT_DESCENT");
  public static final Atom ITALIC_ANGLE 
    = new Atom (ITALIC_ANGLE_ID, "ITALIC_ANGLE");
  public static final Atom X_HEIGHT 
    = new Atom (X_HEIGHT_ID, "X_HEIGHT");
  public static final Atom QUAD_WIDTH 
    = new Atom (QUAD_WIDTH_ID, "QUAD_WIDTH");
  public static final Atom WEIGHT 
    = new Atom (WEIGHT_ID, "WEIGHT");
  public static final Atom POINT_SIZE 
    = new Atom (POINT_SIZE_ID, "POINT_SIZE");
  public static final Atom RESOLUTION 
    = new Atom (RESOLUTION_ID, "RESOLUTION");
  public static final Atom COPYRIGHT 
    = new Atom (COPYRIGHT_ID, "COPYRIGHT");
  public static final Atom NOTICE 
    = new Atom (NOTICE_ID, "NOTICE");
  public static final Atom FONT_NAME 
    = new Atom (FONT_NAME_ID, "FONT_NAME");
  public static final Atom FAMILY_NAME 
    = new Atom (FAMILY_NAME_ID, "FAMILY_NAME");
  public static final Atom FULL_NAME 
    = new Atom (FULL_NAME_ID, "FULL_NAME");
  public static final Atom CAP_HEIGHT 
    = new Atom (CAP_HEIGHT_ID, "CAP_HEIGHT");
  public static final Atom WM_CLASS 
    = new Atom (WM_CLASS_ID, "WM_CLASS");
  public static final Atom WM_TRANSIENT_FOR 
    = new Atom (WM_TRANSIENT_FOR_ID, "WM_TRANSIENT_FOR");


  public Display display;
  public int id;
  public String name;


  /** Predefined. */
  public Atom (int id, String name) {
    this.id = id;
    this.name = name;
  }


  // opcode 16 - intern atom
  /**
   * @see <a href="XInternAtom.html">XInternAtom</a>
   */ 
  private Atom (Display display, String name, boolean only_if_exists) {

    this.display = display;
    this.name = name;

    int n = name.length();
    int p = RequestOutputStream.pad (n);
    
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (16, only_if_exists ? 1 : 0,
                                          2 + (n + p) / 4);
      o.write_int16 (n);
      o.skip (2); // Unused.
      o.write_string8 (name);
      o.write_pad (n);

      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply(o);
        i.skip (8); // Unused + sequence number + reply length.
        id = i.read_int32 ();
        i.skip (20); // Unused.
      }

    }

    display.atom_ids.put (new Integer (id), this);
    display.atom_names.put (name, this);
  }


  // opcode 17 - get atom name
  /**
   * @see <a href="XGetAtomName.html">XGetAtomName</a>
   */
  public Atom (Display display, int id, boolean only_if_exists) {

    this.display = display;
    this.id = id;

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (17, 0, 2);
      o.write_int32 (id);

      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8); // Unused + sequence number + reply length.
        int len = i.read_int16 ();
        i.skip (22);
        name = i.read_string8 (len);
        i.pad (len); // Pad.
      }
    }
    display.atom_ids.put (new Integer (id), this);
    display.atom_names.put (name, this);
  }


  /**
   * @see #Atom(Display, int, boolean)
   */
  public static Object intern (Display display, int id) {
    return intern (display, id, false);
  }


  /**
   * @see #Atom(Display, String, boolean)
   */
  public static Atom intern (Display display, String name) {
    return intern (display, name, false);
  }


  /**
   * @see #Atom(Display, int, boolean)
   */
  public static Object intern (Display display, int id, 
    boolean only_if_exists) {    

    Object value = display.atom_ids.get (new Integer (id));
    if (value != null && value instanceof Atom) return value;
    return new Atom (display, id, only_if_exists);
  }


  /**
   * @see #Atom(Display, String, boolean)
   */
  public static Atom intern (Display display, String name,
                             boolean only_if_exists) { 

    Object value = display.atom_names.get (name);
    if (value != null && value instanceof Atom) return (Atom) value;
    Atom atom = new Atom (display, name, only_if_exists);
    if (atom.id == 0) {
      atom = null;
    }
    return atom;
  }


  public String toString () {    
    return "#Atom: " + name + " " + id;
  }
}
