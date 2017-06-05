package gnu.x11;

import gnu.x11.event.Event;
import gnu.x11.extension.glx.GLXDrawable;


/** X window. */
public class Window extends Drawable implements GLXDrawable {

  /**
   * Predefined windows.
   *
   * <p>All predefined resources are not "properly" initialzied, in the
   * sense that member variable <code>display</code> is <code>null</code>. 
   * That is, they are not connected to any X server, and cannot be used
   * for server interaction (because <code>display == null</code>,
   * resulting in <code>NullPointerException</code>).
   *
   * <p>For special operations like setting selection owner to
   * {@link #NONE}, do:
   *
   * <pre><code>
   *   Window.NONE.display = ...;
   *   Window.NONE.set_selection_owner (...);
   * </code></pre>
   *
   * <p>We could make these predefined resources members of
   * <code>Display</code> class, and could initialize them properly for
   * each <code>Display</code> instance. It would prevent programmers from
   * the mistake described above. However, predefined resources are not
   * designed in the first place for server interaction. For example, you
   * cannot draw a line on <code>Window.POINTER_WINDOW</code>. Second, it
   * is a better classification to put predefined resources in their
   * respective classes. Third, it is cheaper to have predefined resources
   * as <code>static final</code> objects.
   *
   * <p>Note also that, consequently, predefined resources are not interned
   * to <code>Display.resource</code>. For example, <code>Window.intern
   * (this, 0)</code> will not return static variable {@link #NONE}. 
   * Instead it creates a new <code>Window</code> object which sequential
   * intern calls will return. As a side effect, <code>Window</code>
   * equality should not be tested with <code>==</code> operator; it should
   * be tested easily as <code>this_window.id == that_window.id</code>.
   */
  public static final Window NONE = new Window (0);


  public int x, y;
  public Window parent;


  /** Predefined. */
  public Window (int id) { super (id); }

  /** Intern. */
  public Window (Display display, int id) { super (display, id); }


  /**
   * @see #Window(Window, int, int, int, int)
   */
  public Window (Window parent, Rectangle geometry) {
    this (parent, geometry.x, geometry.y, geometry.width, geometry.height);
  }


  /** X window attributes. */
  public static class Attributes extends ValueList {
    public final static Attributes EMPTY = new Attributes ();
    public Attributes () { super (15); }


    /** 
     * @param p possible:
     * {@link Pixmap#NONE} (default),
     * {@link Pixmap#PARENT_RELATIVE} 
     */
    public void set_background (Pixmap p) { set (0, p.id); }


    /** 
     * @see #set_background(int)
     */
    public void set_background (Color c) { set_background (c.pixel); }


    public void set_background (int pixel) { set (1, pixel); }


    /** 
     * @param p possible: {@link Pixmap#COPY_FROM_PARENT} (default)
     */
    public void set_border (Pixmap p) { set (2, p.id); }


    /** 
     * @see #set_border(int)
     */
    public void set_border (Color c) { set_border (c.pixel); }


    public void set_border (int pixel) { set (3, pixel); }

 
    public static final int FORGET = 0;
    public static final int NORTH_WEST = 1;
    public static final int NORTH = 2;
    public static final int NORTH_EAST = 3;
    public static final int WEST = 4;
    public static final int CENTER = 5;
    public static final int EAST = 6;
    public static final int SOUTH_WEST = 7;
    public static final int SOUTH = 8;
    public static final int SOUTH_EAST = 9;
    public static final int STATIC = 10;


    /** 
     * @param i valid:
     * {@link #FORGET} (default),
     * {@link #NORTH_WEST},
     * {@link #NORTH},
     * {@link #NORTH_EAST},
     * {@link #WEST},
     * {@link #CENTER},
     * {@link #EAST},
     * {@link #SOUTH_WEST},
     * {@link #SOUTH},
     * {@link #SOUTH_EAST},
     * {@link #STATIC}
     */
    public void set_win_gravity (int i) { set (5, i); }


    public static final int NOT_USEFUL = 0;
    public static final int WHEN_MAPPED = 1;
    public static final int ALWAYS = 2;


    /**
     * @param i valid:
     * {@link #NOT_USEFUL}(default),
     * {@link #WHEN_MAPPED},
     * {@link #ALWAYS}
     */
    public void set_backing_store (int i) { set (6, i); }


    /** 
     * @param i default: all ones
     */
    public void set_backing_plane (int i) { set (7, i); }


    /** 
     * #set_backing(int)
     */
    public void set_backing (Color c) { set_backing (c.pixel); }


    /** 
     * @param i default: zero
     */
    public void set_backing (int pixel) { set (8, pixel); }


    /** 
     * @param b default: false
     */
    public void set_override_redirect (boolean b) { set (9, b); }


    /** 
     * @param b default: false
     */
    public void set_save_under (boolean b) { set (10, b); }


    /** 
     * @param i default: {}
     */
    public void set_event_mask (int i) { set (11, i); }


    public void add_event_mask (int i) { 
      set_event_mask (event_mask () | i); 
    }


    public int event_mask () { return data [11]; }


    /** 
     * @param i default: {}
     */
    public void set_do_not_propagate_mask (int i) { set (12, i); }


    /** 
     * @param c possible: {@link Colormap#COPY_FROM_PARENT} (default)
     */
    public void set_colormap (Colormap c) { set (13, c.id); }


    /**
     * @param c possible: {@link Cursor#NONE}
     */
    public void set_cursor (Cursor c) { set (14, c.id); }


    public Object clone () {
      Attributes attr = new Attributes ();
      attr.copy (this);
      return attr;
    }
  }


  /**
   * @see #Window(Window, int, int, int, int, int, Window.Attributes)
   */
  public Window (Window parent, Rectangle geometry, int border_width,
    Attributes attr) {
    
    this (parent, geometry.x, geometry.y, geometry.width, geometry.height,
      border_width, attr);
  }


  /**
   * Initialize member fields only without creating object in X server.
   */
  public Window (Window parent, int x, int y, int width, int height) {
    super (parent.display);

    this.parent = parent;
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
  }


  /**
   * @see #create(int, int, int, int, Window.Attributes)
   */
  public Window (Window parent, int x, int y, int width, int height, 
    int border_width, Attributes attr) {
    
    this (parent, x, y, width, height);
    create (border_width, attr);
  }


  public static final int COPY_FROM_PARENT = 0;
  public static final int INPUT_OUTPUT = 1;
  public static final int INPUT_ONLY = 2;


  // opcode 1 - create window
  /**
   * @param depth possible: {@link #COPY_FROM_PARENT}
   
   * @param klass valid:
   * {@link #COPY_FROM_PARENT},
   * {@link #INPUT_OUTPUT},
   * {@link #INPUT_ONLY}
   *
   * @param visual_id possible: {@link #COPY_FROM_PARENT}
   * @see <a href="XCreateWindow.html">XCreateWindow</a>
   */  
  public void create (int border_width, int depth, int klass, int visual_id,
                      Attributes attr) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (1, depth, 8 + attr.count ());
      o.write_int32 (id);
      o.write_int32 (parent.id);
      o.write_int16 (x);
      o.write_int16 (y);
      o.write_int16 (width);
      o.write_int16 (height);
      o.write_int16 (border_width);
      o.write_int16 (klass);
      o.write_int32 (visual_id);
      o.write_int32 (attr.bitmask);
      attr.write (o);
      o.send ();
    }
  }


  /**
   * @see #create(int, int, int, int, Window.Attributes)
   */  
  public void create (int border_width, Attributes attr) {
    create (border_width, COPY_FROM_PARENT, COPY_FROM_PARENT,
      COPY_FROM_PARENT, attr);
  }


  /**
   * @see #create(int, Window.Attributes)
   */  
  public void create () {
    create (0, Attributes.EMPTY);
  }


  // opcode 2 - change window attributes
  /**
   * This request will be aggregated.
   *
   * @see <a href="XChangeAttributes.html">
   * XChangeAttributes</a>
   *
   * @see Request.Aggregate aggregation
   */  
  public void change_attributes (Attributes attr) {
    // FIXME: Implement aggregation.
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (2, 0, 3 + attr.count ());
      o.write_int32 (id);
      o.write_int32 (attr.bitmask);
      attr.write (o);
      o.send ();
    }
  }


  /** Reply of {@link #attributes()}. */
  public static class AttributesReply {

    public int backing_store;

    public int visual_id;

    public int window_class;

    public int bit_gravity;

    public int win_gravity;

    public int backing_planes;

    public int backing_pixel;

    public boolean save_under;

    public boolean map_is_installed;

    public int map_state;

    public boolean override_redirect;

    public int colormap_id;

    public int all_event_masks;

    public int your_event_mask;

    public int do_not_propagate_mask;

    /**
     * Reads the AttributesReply data from the specified input stream.
     */
    public AttributesReply (ResponseInputStream in) {

      int code = in.read_int8 ();
      assert code == 1 : "Errors and events should be catched in Connection";

      backing_store = in.read_int8 ();

      in.read_int16 (); // Sequence number, not needed.

      in.read_int32 (); // Reply length, not needed.

      visual_id = in.read_int32 ();
      window_class = in.read_int16 ();
      bit_gravity = in.read_int8 ();
      win_gravity = in.read_int8 ();
      backing_planes = in.read_int32 ();
      backing_pixel = in.read_int32 ();
      save_under = in.read_bool ();
      map_is_installed = in.read_bool ();
      map_state = in.read_int8 ();
      override_redirect = in.read_bool ();
      colormap_id = in.read_int32 ();
      all_event_masks = in.read_int32 ();
      your_event_mask = in.read_int32 ();
      do_not_propagate_mask = in.read_int16 ();
      in.skip (2); // Unused.
    }
  
    public static final int UNMAPPED = 0;
    public static final int UNVIEWABLE = 1;
    public static final int VIEWABLE = 2;
  
  
    /**
     * @return valid:
     * {@link #UNMAPPED},
     * {@link #UNVIEWABLE},
     * {@link #VIEWABLE}
     */
    public int map_state () {
      return map_state;
    }
  
  
    public boolean override_redirect () {
      return override_redirect;
    }
  }
  
  
  // opcode 3 - get window attributes
  /**
   * @see <a href="XGetAttributes.html">XGetAttributes</a>
   */
  public AttributesReply attributes () {
    RequestOutputStream o = display.out;
    AttributesReply r;
    synchronized (o) {
      o.begin_request (3, 0, 2);
      o.write_int32 (id);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        r = new AttributesReply (i);
      }
    }
    return r;
  }


  // opcode 4 - destroy window
  /**
   * @see <a href="XDestroyWindow.html">XDestroyWindow</a>
   */
  public void destroy () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (4, 0, 2);
      o.write_int32 (id);
      o.send ();
    }
  }


  // opcode 5 - destroy subwindows
  /**
   * @see <a href="XDestroySubwindows.html">XDestroySubwindows</a>
   */
  public void destroy_subwindows () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (5, 0, 2);
      o.write_int32 (id);
      o.send ();
    }
  }  


  public static final int INSERT = 0;
  public static final int DELETE = 1;


  // opcode 6 - change save set
  /**
   * @param mode valid:
   * {@link #INSERT},
   * {@link #DELETE}
   *
   * @see <a href="XChangeSaveSet.html">XChangeSaveSet</a>
   */  
  public void change_save_set (boolean mode) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (6, mode ? 1 : 0, 2);
      o.write_int32 (id);
      o.send ();
    }
  }


  // opcode 7 - reparent window
  /**
   * @see <a href="XReparentWindow.html">XReparentWindow</a>
   */
  public void reparent (Window parent, int x, int y) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (7, 0, 4);
      o.write_int32 (id);
      o.write_int32 (parent.id);
      o.write_int16 (x);
      o.write_int16 (y);
      o.send ();
    }
  }


  // opcode 8 - map window
  /**
   * @see <a href="XMapWindow.html">XMapWindow</a>
   */
  public void map () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (8, 0, 2);
      o.write_int32 (id);
      o.send ();
      o.flush ();
    }
  }


  // opcode 9 - map subwindows
  /**
   * @see <a href="XMapSubwindows.html">XMapSubwindows</a>
   */
  public void map_subwindows () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (9, 0, 2);
      o.write_int32 (id);
      o.send ();
    }
  }  


  // opcode 10 - unmap window
  /**
   * @see <a href="XUnmapWindow.html">XUnmapWindow</a>
   */
  public void unmap () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (10, 0, 2);
      o.write_int32 (id);
      o.send ();
    }
  }


  // opcode 11 - unmap subwindows
  /**
   * @see <a href="XUnmapSubwindows.html">XUnmapSubwindows</a>
   */
  public void unmap_subwindows () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (11, 0, 2);
      o.write_int32 (id);
      o.send ();
    }
  }


  /** X window changes. */
  public static class Changes extends ValueList {
    public Changes () { super (7); } 
    public void x (int i) { set (0, i); }
    public void y (int i) { set (1, i); }
    public void width (int i) { set (2, i); }
    public void height (int i) { set (3, i); }
    public void border_width (int i) { set (4, i); }
    public void sibling_id (int i) { set (5, i); }
    public void sibling (Window window) { sibling_id (window.id); }
  
  
    public static final int ABOVE = 0;
    public static final int BELOW = 1;
    public static final int TOP_IF = 2;
    public static final int BOTTOM_IF = 3;
    public static final int OPPOSITE = 4;
  
  
    /**
     * @param i valid:
     * {@link #ABOVE},
     * {@link #BELOW},
     * {@link #TOP_IF},
     * {@link #BOTTOM_IF},
     * {@link #OPPOSITE}
     */
    public void stack_mode (int i) { set (6, i); }
  
  
    public Object clone () {
      Changes changes = new Changes ();
      changes.copy (this);
      return changes;
    }
  }


  // opcode 12 - configure window
  /**
   * This request will be aggregated.
   *
   * @see <a href="XConfigureWindow.html">XConfigureWindow</a>
   * @see Request.Aggregate aggregation
   */
  public void configure (Changes changes) {
    // FIXME: Implement aggregation.
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (12, 0, 3 + changes.count ());
      o.write_int32 (id);
      o.write_int16 (changes.bitmask);
      o.skip (2);
      changes.write (o);
      o.send ();
    }
  }


  public static final int RAISE_LOWEST = 0;
  public static final int LOWER_HIGHEST = 1;


  // opcode 13 - circulate window
  /**
   * @param direction valid:
   * {@link #RAISE_LOWEST},
   * {@link #LOWER_HIGHEST}
   * 
   * @see <a href="XCirculateSubwindows.html">XCirculateSubwindows</a>
   */
  public void circulate_window (int direction) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (13, direction, 2);
      o.write_int32 (id);
      o.send ();
    }
  }

  /** Reply of {@link #tree()}. */
  public class TreeInfo {

    Window root;
    Window parent;
    private Window[] children;

    TreeInfo (ResponseInputStream i) {
      root = (Window) intern (display, i.read_int32 ());
      int parent_id = i.read_int32 ();
      if (parent_id != 0)
        parent = (Window) intern (display, parent_id);
      else
        parent = null;

      int num_windows = i.read_int16 ();
      i.skip (14);
      children = new Window [num_windows];
      for (int j = 0; j < num_windows; j++) {
        int id = i.read_int32 ();
        children [j] = (Window) intern (display, id);
      }
    }

    public Window[] children ()
    {
      return children;
    }
  }


  // opcode 15 - query tree
  /**
   * @see <a href="XQueryTree.html">XQueryTree</a>
   */
  public TreeInfo tree () {

    TreeInfo info;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (15, 0, 2);
      o.write_int32 (id);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        info = new TreeInfo (i);
      }
    }
    return info;
  }


  public static final int REPLACE = 0;
  public static final int PREPEND = 1;
  public static final int APPEND = 2;


  // opcode 18 - change property
  /**
   * Extra parameters (offset and data_format) are used to support Data
   * class as parameter for writing. See set_wm_normal_hints ().
   *
   * @param mode valid:
   * {@link #REPLACE},
   * {@link #PREPEND},
   * {@link #APPEND}
   * 
   * @param format: valid:
   * <code>8</code>,
   * <code>16</code>,
   * <code>32</code>
   *
   * @param data_format: valid:
   * <code>8</code>,
   * <code>16</code>,
   * <code>32</code>
   * 
   * @see <a href="XChangeProperty.html">XChangeProperty</a>
   */ 
  public void change_property (int mode, Atom property, Atom type,
                               int format, Object data, int offset,
                               int data_format) {

    byte [] byteData;
    switch (format) {
      case 8:
        byteData = (byte []) data;
        break;
      case 16:
        short [] shortData = (short []) data;
        byteData = new byte [shortData.length * 2];
        for (int i = 0; i < shortData.length; i++) {
          byteData [i * 2] = (byte) (shortData [i] >> 8);
          byteData [i * 2 + 1] = (byte) (shortData [i]);
        }
        break;
      case 32:
        int [] intData = (int []) data;
        byteData = new byte [intData.length * 4];
        for (int i = 0; i < intData.length; i++) {
          byteData [i * 4] = (byte) (intData [i] >> 24);
          byteData [i * 4 + 1] = (byte) (intData [i] >> 16);
          byteData [i * 4 + 2] = (byte) (intData [i] >> 8);
          byteData [i * 4 + 3] = (byte) (intData [i]);
        }
        break;
      default:
        throw new IllegalArgumentException("Illegal format argument: "
                                           + format);
    }
    int len = 0;
    int n = byteData.length;
    switch (format) {
    case 8:
      len = n;
      break;
    case 16:
      len = n / 2;
      break;
    case 32:
      len = n / 4;
      break;
    default:
      len = 0;
    }

    int p = RequestOutputStream.pad (n);

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (18, mode, 6 + (n + p) / 4);
      o.write_int32 (id);
      o.write_int32 (property.id);
      o.write_int32 (type.id);
      o.write_int8 (format);
      o.skip (3);
      o.write_int32 (len); // data length in format unit
      o.write (byteData);
      o.skip (p);
      o.send ();
    }
  }


  // opcode 19 - delete property
  /**
   * @see <a href="XDeleteProperty.html">XDeleteProperty</a>
   */
  public void delete_property (Atom property) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (19, 0, 3);
      o.write_int32 (id);
      o.write_int32 (property.id);
      o.send ();
    }
  }


  /** Reply of {@link #property(boolean, Atom, Atom, int, int)}. */
  public class Property {

    private int format;
    private int type_id;
    private int bytes_after;
    private int length;
    private byte [] value;

    Property (ResponseInputStream i) {
      format = i.read_int8 ();
      i.skip (6);
      type_id = i.read_int32 ();
      bytes_after = i.read_int32 ();
      length = i.read_int32 ();
      i.skip (12);
      int num_bytes;
      switch (format) {
      case 8:
        num_bytes = length;
        break;
      case 16:
        num_bytes = length * 2;
        break;
      case 32:
        num_bytes = length * 4;
        break;
      default:
        num_bytes = 0;
      }
      value = new byte [num_bytes];
      i.read_data (value);
      int p = RequestOutputStream.pad (num_bytes);
      if (p > 0)
        i.skip (p);
    }

    public int format () {
      return format;
    }

    public int type_id () {
      return type_id;
    }

    /**
     * Returns the value at index <code>i</code>. This interprets the
     * underlying byte data according to the format of this property.
     *
     * @param i the index
     *
     * @return the value at the specified index
     */
    public int value (int i) {
      int v;
      switch (format) {
      case 8:
        v = value [i];
        break;
      case 16:
        v = ((0xff & value [i * 2]) << 8) | (0xff & value [i * 2 + 1]);
        break;
      case 32:
        v =   ((0xff & value [i * 4]) << 24)
            | ((0xff & value [i * 4 + 1]) << 16)
            | ((0xff & value [i * 4 + 2] << 8))
            |  (0xff & ((int) value [i * 4 + 3]));
        break;
      default:
        throw new ArrayIndexOutOfBoundsException();
      }
      return v;
    }

    /**
     * Returns the property value as string.
     *
     * @return the property value as string
     */
    public String string_value () {
      return new String (value);
    }
  }
  
  
  // opcode 20 - get property
  /**
   * @see <a href="XGetWindowProperty.html">XGetWindowProperty</a>
   */
  public Property get_property (boolean delete, Atom property,
                                Atom type, int offset, int length) {

    Property prop;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (20, delete ? 1 : 0, 6);
      o.write_int32 (id);
      o.write_int32 (property.id);
      o.write_int32 (type.id);
      o.write_int32 (offset);
      o.write_int32 (length);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        prop = new Property (i);
      }
    }
    return prop;
  }


  // opcode 21 - list properties
  /**
   * @return valid:
   * {@link Enum#next()} of type {@link Atom},
   * {@link Enum#next4()}
   *
   * @see <a href="XRotateWindowProperties.html">
   * XRotateWindowProperties</a>
   */
  public Atom [] properties () {

    Atom [] atoms;
    RequestOutputStream o = display.out;
    int[] atomIds;
    synchronized (o) {
      o.begin_request (21, 0, 2);
      o.write_int32 (id);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        int num_atoms = i.read_int16 ();
        atomIds = new int [num_atoms];
        i.skip (22);
        for (int j = 0; j < num_atoms; j++) {
          atomIds [j] = i.read_int32 ();
        }
      }
    }
    atoms = new Atom [atomIds.length];
    for (int i = 0; i < atomIds.length; i++)
      atoms[i] = (Atom) Atom.intern (display, atomIds [i]);
    return atoms;
  }


  // opcode 22 - set selection owner
  /**
   * @param time possible: {@link Display#CURRENT_TIME}
   * @see <a href="XSetSelectionOwner.html">XSetSelectionOwner</a>
   */
  public void set_selection_owner (Atom selection, int time) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (22, 0, 4);
      o.write_int32 (id);
      o.write_int32 (selection.id);
      o.write_int32 (time);
      o.send ();
    }
  }


  // opcode 24 - convert selection
  /**
   * @param time possible: {@link Display#CURRENT_TIME}
   * @see <a href="XConvertSelection.html">XConvertSelection</a>
   */
  public void convert_selection (Atom selection, Atom target, 
                                 Atom property, int time) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (24, 0, 6);
      o.write_int32 (id);
      o.write_int32 (selection.id);
      o.write_int32 (target.id);
      o.write_int32 (property.id);
      o.write_int32 (time);
      o.send ();
    }
  }


  // opcode 25 - send event
  /**
   * @see <a href="XSendEvent.html">XSendEvent</a>
   */
  public void send_event (boolean propagate, int event_mask, 
                          Event event) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (25, propagate ? 1 : 0, 11);
      o.write_int32 (id);
      o.write_int32 (event_mask);
      event.write (o);
      o.send ();
    }
  }


  public static final int SYNCHRONOUS = 0;
  public static final int ASYNCHRONOUS = 1;


  public static final int SUCCESS = 0;
  public static final int ALREADY_GRABBED = 1;
  public static final int INVALID_TIME = 2;
  public static final int NOT_VIEWABLE = 3;
  public static final int FROZEN = 4;


  // opcode 26 - grab pointer
  /**
   * @param pointer_mode valid:
   * {@link #SYNCHRONOUS},
   * {@link #ASYNCHRONOUS}
   * 
   * @param keyboard_mode valid:
   * {@link #SYNCHRONOUS},
   * {@link #ASYNCHRONOUS}
   * 
   * @param confine_to possible: {@link #NONE}
   * @param cursor possible: {@link Cursor#NONE}
   * @param time possible: {@link Display#CURRENT_TIME}
   *
   * @return valid:
   * {@link #SUCCESS},
   * {@link #ALREADY_GRABBED},
   * {@link #FROZEN},
   * {@link #INVALID_TIME},
   * {@link #NOT_VIEWABLE}
   *
   * @see <a href="XGrabPointer.html">XGrabPointer</a>
   */
  public int grab_pointer (boolean owner_events, int event_mask, 
    int pointer_mode, int keyboard_mode, Window confine_to, Cursor cursor,
    int time) { 

    int status;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (26, owner_events ? 1 : 0, 6);
      o.write_int32 (id);
      o.write_int16 (event_mask);
      o.write_int16 (pointer_mode);
      o.write_int16 (keyboard_mode);
      o.write_int32 (confine_to.id);
      o.write_int32 (cursor.id);
      o.write_int32 (time);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        status = i.read_int8 ();
        i.skip (30);
      }
    }
    return status;  
  }


  public static final int ANY_BUTTON = 0;
  public static final int ANY_MODIFIER = 0x8000;


  // opcode 28 - grab button
  /**
   * @param button possible: {@link #ANY_BUTTON}
   * @param modifiers possible: {@link #ANY_MODIFIER}
   * @param pointer_mode valid:
   * {@link #SYNCHRONOUS},
   * {@link #ASYNCHRONOUS}
   * 
   * @param keyboard_mode valid:
   * {@link #SYNCHRONOUS},
   * {@link #ASYNCHRONOUS}
   * 
   * @param confine_to possible: {@link #NONE}
   * @param cursor possible: {@link Cursor#NONE}
   * @see <a href="XGrabButton.html">XGrabButton</a>
   */
  public void grab_button (int button, int modifiers, boolean owner_events,
                           int event_mask, int pointer_mode, int keyboard_mode,
                           Window confine_to, Cursor cursor) { 

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (28, owner_events ? 1 : 0, 6);
      o.write_int32 (id);
      o.write_int16 (event_mask);
      o.write_int8 (pointer_mode);
      o.write_int8 (keyboard_mode);
      o.write_int32 (confine_to.id);
      o.write_int32 (cursor.id);
      o.write_int8 (button);
      o.skip (1);
      o.write_int16 (modifiers);
      o.send ();
    }
  }


  // opcode 29 - ungrab button
  /**
   * @param button possible: {@link #ANY_BUTTON}
   * @param modifiers possible: {@link #ANY_MODIFIER}
   * @see <a href="XUngrabButton.html">XUngrabButton</a>
   */
  public void ungrab_button (int button, int modifiers) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (29, button, 3);
      o.write_int32 (id);
      o.write_int16 (modifiers);
      o.skip (2);
      o.send ();
    }
  }
  

  // opcode 31 - grab keyboard
  /**
   * @param pointer_mode valid:
   * {@link #SYNCHRONOUS},
   * {@link #ASYNCHRONOUS}
   * 
   * @param keyboard_mode valid: 
   * {@link #SYNCHRONOUS},
   * {@link #ASYNCHRONOUS}
   * 
   * @param time possible: {@link Display#CURRENT_TIME}
   *
   * @return valid:
   * {@link #SUCCESS},
   * {@link #ALREADY_GRABBED},
   * {@link #FROZEN},
   * {@link #INVALID_TIME},
   * {@link #NOT_VIEWABLE}
   *
   * @see <a href="XGrabKeyboard.html">XGrabKeyboard</a>
   */
  public int grab_keyboard  (boolean owner_events, int pointer_mode, 
    int keyboard_mode, int time) { 

    int status;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (31, owner_events ? 1 : 0, 4);
      o.write_int32 (id);
      o.write_int32 (time);
      o.write_int8 (pointer_mode);
      o.write_int8 (keyboard_mode);
      o.skip (2);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        status = i.read_int8 ();
        i.skip (30);
      }
    }
    return status;
  }

  
  // opcode 33 - grab key
  /**
   * @param modifiers possible: {@link #ANY_MODIFIER}
   * @param pointer_mode valid: 
   * {@link #SYNCHRONOUS},
   * {@link #ASYNCHRONOUS}
   * 
   * @param keyboard_mode valid:
   * {@link #SYNCHRONOUS},
   * {@link #ASYNCHRONOUS}
   * 
   * @see <a href="XGrabKey.html">XGrabKey</a>
   */
  public void grab_key (int keysym, int modifiers, boolean owner_events, 
                        int pointer_mode, int keyboard_mode) {

    int keycode = display.input.keysym_to_keycode (keysym);

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (33, owner_events ? 1 : 0, 4);
      o.write_int32 (id);
      o.write_int16 (modifiers);
      o.write_int8 (keycode);
      o.write_int8 (pointer_mode);
      o.write_int8 (keyboard_mode);
      o.send ();
    }
  }


  public static final int ANY_KEY = 0;  


  // opcode 34 - ungrab key
  /**
   * @param key possible: {@link #ANY_KEY}
   * @param modifiers possible: {@link #ANY_MODIFIER}
   * @see <a href="XUngrabKey.html">XUngrabKey</a>
   */
  public void ungrab_key (int keysym, int modifiers) {
    int keycode = keysym == 0 ? 0 
      : display.input.keysym_to_keycode (keysym);

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (34, keycode, 3);
      o.write_int32 (id);
      o.write_int16 (modifiers);
      o.skip (2);
      o.send ();
    }
  }


  /** Reply of {@link #pointer()}. */
  public class PointerInfo {

    public boolean same_screen; 
    public Window root;
    public Window child;

    public int root_x;
    public int root_y;

    public int win_x;
    public int win_y;

    public int mask;

    PointerInfo (ResponseInputStream i) {
      same_screen = i.read_bool ();
      i.skip (6);

      int root_id = i.read_int32 ();
      root = (Window) intern (display, root_id);

      int child_id = i.read_int32 ();
      if (child_id != 0)
        child = (Window) intern (display, root_id);
      else
        child = null;

      root_x = i.read_int16 ();
      root_y = i.read_int16 ();
      win_x = i.read_int16 ();
      win_y = i.read_int16 ();
      mask = i.read_int16 ();
    }

    public Point root_position () {
      return new Point (root_x, root_y);
    }
  }
  
  
  // opcode 38 - query pointer
  /**
   * @see <a href="XQueryPointer.html">XQueryPointer</a>
   */
  public PointerInfo query_pointer () {

    PointerInfo info;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (38, 0, 2);
      o.write_int32 (id);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        info = new PointerInfo (i);
        i.skip (6);
      }
    }
    return info;
  }

  
  public static class TimeCoord {

    public long timestamp;
    public int x;
    public int y;

    TimeCoord (ResponseInputStream i) {
      timestamp = i.read_int32 ();
      x = i.read_int16 ();
      y = i.read_int16 ();
    }
  }

  // opcode 39 - get motion events
  /**
   * @param start possible: {@link Display#CURRENT_TIME}
   * @param stop possible: {@link Display#CURRENT_TIME}
   * @see <a href="XGetMotionEvents.html">XGetMotionEvents</a>
   */
  public TimeCoord [] get_motion_events (int start, int stop) {

    TimeCoord [] timecoords;

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (39, 0, 4);
      o.write_int32 (id);
      o.write_int32 (start);
      o.write_int32 (stop);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        int len = i.read_int32 ();
        timecoords = new TimeCoord [len];
        i.skip (20);
        for (int j = 0; j < len; j++)
          timecoords [j] = new TimeCoord (i);
      }
    }
    return timecoords;
  }


  
  /** Reply of {@link #translate_coordinates(Window, int, int)}. */
  public class Coordinates {
    boolean same_screen;
    Window child;
    public int x;
    public int y;
    Coordinates (ResponseInputStream i) {
      same_screen = i.read_bool ();
      i.skip (6);
      int child_id = i.read_int32 ();
      if (child_id != 0)
        child = (Window) intern (display, child_id);
      else
        child = null;
      x = i.read_int16 ();
      y = i.read_int16 ();
    }
  }

  // opcode 40 - translate coordinates
  /**
   * @see <a href="XTranslateCoordinates.html">XTranslateCoordinates</a>
   */
  public Coordinates translate_coordinates (Window src, int src_x, int src_y) {

    Coordinates coords;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (40, 0, 4);
      o.write_int32 (src.id);
      o.write_int32 (id);
      o.write_int16 (src_x);
      o.write_int16 (src_y);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        coords = new Coordinates (i);
        i.skip (16);
      }
    }
    return coords;
  }


  // opcode 41 - warp pointer
  /**
   * @param src possible: {@link #NONE}
   * @see <a href="XWarpPointer.html">XWarpPointer</a>
   */
  public void warp_pointer (Window src, int src_x, int src_y, 
                            int src_width, int src_height, int dest_x, int dest_y) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (41, 0, 6);
      o.write_int32 (src.id);
      o.write_int32 (id);
      o.write_int16 (src_x);
      o.write_int16 (src_y);
      o.write_int16 (src_width);
      o.write_int16 (src_height);
      o.write_int16 (dest_x);
      o.write_int16 (dest_y);
      o.send ();
    }
  }


  /**
   * @see #change_attributes(Window.Attributes)
   * @see Attributes#set_background(Color)
   */
  public void set_background (Color c) {
    Attributes attr = new Attributes ();
    attr.set_background (c);
    change_attributes (attr);
  }


  /**
   * @see #change_attributes(Window.Attributes)
   * @see Attributes#set_background(int)
   */
  public void set_background (int pixel) {
    Attributes attr = new Attributes ();
    attr.set_background (pixel);
    change_attributes (attr);
  }


  /**
   * @see #change_attributes(Window.Attributes)
   * @see Attributes#set_background(Pixmap)
   */
  public void set_background (Pixmap p) {
    Attributes attr = new Attributes ();
    attr.set_background (p);
    change_attributes (attr);
  }


  /**
   * @see #change_attributes(Window.Attributes)
   * @see Attributes#set_border(Color)
   */
  public void set_border (Color c) {
    Attributes attr = new Attributes ();
    attr.set_border (c);
    change_attributes (attr);
  }


  /**
   * @see #change_attributes(Window.Attributes)
   * @see Attributes#set_border(int)
   */
  public void set_border (int pixel) {
    Attributes attr = new Attributes ();
    attr.set_border (pixel);
    change_attributes (attr);
  }


  /**
   * @see #change_attributes(Window.Attributes)
   * @see Attributes#set_border(Pixmap)
   */
  public void set_border (Pixmap p) {
    Attributes attr = new Attributes ();
    attr.set_border (p);
    change_attributes (attr);
  }


  /**
   * @see #change_attributes(Window.Attributes)
   * @see Attributes#set_colormap(Colormap)
   */
  public void set_colormap (Colormap cmap) {
    Attributes attr = new Attributes ();
    attr.set_colormap (cmap);
    change_attributes (attr);
  }


  public static final Window POINTER_ROOT = new Window (1);


  public static final int TO_NONE = 0;
  public static final int TO_POINTER_ROOT = 1;
  public static final int TO_PARENT = 2;


  // opcode 42 - set input focus
  /**
   * @param mode valid:
   * {@link #TO_NONE},
   * {@link #TO_POINTER_ROOT},
   * {@link #TO_PARENT}
   * 
   * @param time possible: {@link Display#CURRENT_TIME}
   * @see <a href="XSetInputFocus.html">XSetInputFocus</a>
   */
  public void set_input_focus (int revert_to, int time) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (43, revert_to, 3);
      o.write_int32 (id);
      o.write_int32 (time);
      o.send ();
    }
  }


  // opcode 61 - clear area
  /**
   * @see <a href="XClearArea.html">XClearArea</a>
   */
  public void clear_area (int x, int y, int width, int height, 
                          boolean exposures) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (61, exposures ? 1 : 0, 4);
      o.write_int32 (id);
      o.write_int16 (x);
      o.write_int16 (y);
      o.write_int16 (width);
      o.write_int16 (height);
      o.send ();
    }
  }


  // opcode 83 - list installed colormaps
  /**
   * @return valid:
   * {@link Enum#next()} of type {@link Colormap},
   * {@link Enum#next4()}
   *
   * @see <a href="XListInstalledColormaps.html">
   * XListInstalledColormaps</a>
   */
  public Colormap [] list_installed_colormaps () {

    Colormap [] maps;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (83, 0, 2);
      o.write_int32 (id);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        int num_maps = i.read_int16 ();
        maps = new Colormap [num_maps];
        i.skip (22);
        for (int j = 0; j < num_maps; j++) {
          int id = i.read_int32 ();
          maps [j] = (Colormap) Colormap.intern (display, id);
        }
      }
    }
    return maps;
  }


  // opcode 114 - rotate properties
  /**
   * @see <a href="XRotateWindowProperties.html">
   * XRotateWindowProperties</a>
   */
  public void rotate_properties (Atom [] properties, int delta) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (114, 0, 3 + properties.length);
      o.write_int32 (id);
      o.write_int16 (properties.length);
      o.write_int16 (delta);
      
      for (int i = 0; i < properties.length; i++)
        o.write_int32 (properties [i].id);

      o.send ();
    }
  }


  /**
   * @see #change_property(int, int, Atom, Atom, int, Object, int, int)
   */
  public void change_property (Atom property, Atom type, int data) {   
    change_property (REPLACE, property, type, 32, 
      new byte [] {(byte) (data >> 24), (byte) (data >> 16),
                   (byte) (data >> 8), (byte) data}, 0, 32);
  }


  /**
   * @see <a href="XClearWindow.html">XClearWindow</a>
   * @see #clear_area(int, int, int, int, boolean)
   */
  public void clear (boolean exposures) {
    clear_area (0, 0, width, height, exposures);
  }


  public void delete () {
    // FIXME: Re-think WM -messages.
//    if (!(wm_protocol ("WM_DELETE_WINDOW"))) return;
//
//    ClientMessage event = new ClientMessage (display);
//    Atom wm_protocols = (Atom) Atom.intern (display, "WM_PROTOCOLS");
//    Atom wm_delete_window = (Atom) Atom.intern (display,
//      "WM_DELETE_WINDOW"); 
//
//    event.set_format (32);
//    event.set_window (this);
//    event.set_type (wm_protocols);
//    event.set_wm_data (wm_delete_window.id);
//    event.set_wm_time (Display.CURRENT_TIME);
//    send_event (false, Event.NO_EVENT_MASK, event);
  }


  /** 
   * @see #configure(Window.Changes)
   */
  public void flip () {
    Changes changes = new Changes ();
    changes.stack_mode (Changes.OPPOSITE);
    configure (changes);
  }


  /** 
   * Grab button ignoring caps lock (LOCK), num lock (MOD2), and scroll
   * lock (MOD5).
   *
   * @see #grab_button(int, int, boolean, int, int, int, Window, Cursor)
   */
  public void grab_button_ignore_locks (int button, int modifiers, 
    boolean owner_events, int event_mask, int pointer_mode, 
    int keyboard_mode, Window confine_to, Cursor cursor) {

    // Are there a portable way to do this?
    // Sawfish and Icewm use the same technique as well.
    // TODO highly inefficient (many X requests)
    for (int i=0; i<Input.LOCK_COMBINATIONS.length; i++)
      grab_button (button, modifiers | Input.LOCK_COMBINATIONS [i],
        owner_events, event_mask, pointer_mode, keyboard_mode, confine_to,
        cursor); 
  }


  /** 
   * Grab key ignoring caps lock (LOCK), num lock (MOD2), and scroll lock
   * (MOD5).
   *
   * @see #grab_key(int, int, boolean, int, int)
   *
   * @see #grab_button_ignore_locks(int, int, boolean, int, int, int,
   * Window, Cursor)
   */
  public void grab_key_ignore_locks (int keysym, int modifiers,
    boolean owner_events, int pointer_mode, int keyboard_mode) {

    for (int i=0; i<Input.LOCK_COMBINATIONS.length; i++)
      grab_key (keysym, modifiers | Input.LOCK_COMBINATIONS [i],
        owner_events, pointer_mode, keyboard_mode);
  }


  /**
   * @see <a href="XIconifyWindow.html">XIconifyWindow</a>
   * @see <a href="icccm.html#4.1.4">ICCCM Section 4.1.4</a>
   * @see #send_event(boolean, int, Event)
   */
  public void iconify () {
    // FIXME: Re-think WM -messages.
//    Atom wm_change_state = (Atom) Atom.intern (display, "WM_CHANGE_STATE");
//
//    ClientMessage event = new ClientMessage (display);
//    event.set_format (32);
//    event.set_window (this);
//    event.set_type (wm_change_state);
//    event.set_wm_data (WMHints.ICONIC);
//    send_event (false, Event.SUBSTRUCTURE_REDIRECT_MASK
//      | Event.SUBSTRUCTURE_NOTIFY_MASK, event); 
  }



  public static Object intern (Display display, int id) {
    Object value = display.resources.get (new Integer (id));
    if (value != null) return value;
    return new Window (display, id);
  }


  /** 
   * @see <a href="XLowerWindow.html">XLowerWindow</a>
   * @see #configure(Window.Changes)
   */
  public void lower () {
    Changes changes = new Changes ();
    changes.stack_mode (Changes.BELOW);
    configure (changes);
  }


  /*
   * @see <a href="XMoveWindow.html">XMoveWindow</a>
   * @see #configure(Window.Changes)
   */
  public void move () {
    Changes changes = new Changes ();
    changes.x (x);
    changes.y (y);
    configure (changes);
  }


  /**
   * @see <a href="XMoveWindow.html">XMoveWindow</a>
   * @see #configure(Window.Changes)
   */
  public void move (int x, int y) {
    if (this.x == x && this.y == y) return;

    this.x = x;
    this.y = y;
    move ();
  }


  /** 
   * @see <a href="XMoveResizeWindow.html">XMoveResizeWindow</a>
   * @see #configure(Window.Changes)
   */
  public void move_resize () {
    move ();
    resize ();
  }


  /** 
   * @see <a href="XMoveResizeWindow.html">XMoveResizeWindow</a>
   * @see #configure(Window.Changes)
   */
  public void move_resize (int x, int y, int width, int height) {
    move (x, y);
    resize (width, height);
  }


  /** 
   * @see <a href="XMoveResizeWindow.html">XMoveResizeWindow</a>
   * @see #configure(Window.Changes)
   */
  public void move_resize (Rectangle rectangle) {
    move_resize (rectangle.x, rectangle.y, rectangle.width, rectangle.height);
  }


  /**
   * @see Display#kill_client(Resource)
   */
  public void kill () {
    display.kill_client (this);
  }


  /** 
   * @see #configure(Window.Changes)
   */
  public void raise () {
    Changes changes = new Changes ();
    changes.stack_mode (Changes.ABOVE);
    configure (changes);
  }


  public Rectangle rectangle () {
    return new Rectangle (x, y, width, height);
  }


  /**
   * @see <a href="XResizeWindow.html">XResizeWindow</a>
   * @see #configure(Window.Changes)
   */
  public void resize () {
    Changes changes = new Changes ();

    // width/height == 0 causes BAD_VALUE Error
    if (width != 0) changes.width (width);
    if (height != 0) changes.height (height);
    if (changes.bitmask != 0) configure (changes);
  }


  /** 
   * @see <a href="XResizeWindow.html">XResizeWindow</a>
   * @see #configure(Window.Changes)
   */
  public void resize (int width, int height) {
    if (this.width == width && this.height == height) return;

    this.width = width;
    this.height = height;
    resize ();
  }


  public boolean resized (Rectangle r) {
    return r.width != width || r.height != height;
  }


  public Screen screen () {
    for (int i=0; i<display.screens.length; i++) {
      Screen screen = display.screens [i];
      if (screen.root_id == id) return screen;
    }

    return null;
  }



  /**
   * @see <a href="XSelectInput.html">XSelectInput</a>
   * @see #change_attributes(Window.Attributes)
   */
  public void select_input (int event_mask) {
    Attributes attr = new Attributes ();
    attr.set_event_mask (event_mask);
    change_attributes (attr);
  }


  /**
   * @see #set_input_focus(int, int)
   */
  public void set_input_focus () {
    set_input_focus (TO_POINTER_ROOT, Display.CURRENT_TIME);
  }

  
  public void set_geometry_cache (Rectangle r) {
    x = r.x;
    y = r.y;
    width = r.width;
    height = r.height;
  }


  /**
   * A standard way to set wm class hint and name in Java.
   *
   * @see #set_wm_class_hint(String, String)
   * @see #set_wm_name(String)
   */
  public void set_wm (Object app, String topic) {
    String res_class = app.getClass ().getName ();
    set_wm_class_hint (topic, res_class);
    set_wm_name (topic + " - " + res_class);
  }    


  /** X window manager class hint. */
  public static class WMClassHint {
    public String res;
    public int middle;
  
  
    public WMClassHint (Data data) { 
      int len = data.read4 (16)-1;
      res = data.read_string (32, len);
      middle = res.indexOf (0);
    }
  
  
    public boolean class_equals (String res_class) {
      if (res_class == null) return false;
      return res.endsWith (res_class);
    }
  
  
    public boolean class_equals (WMClassHint hint) {
      if (hint == null) return false;
      return class_equals (hint.res_class ());
    }
  
  
    public boolean equals (WMClassHint hint) {
      if (hint == null) return false;
      return res.equals (hint.res);
    }
  
  
    public boolean equals (String res_name, String res_class) {
      if (res_name == null || res_class == null) return false;
      if (res_name.length () + res_class.length () != res.length ()-1)
        return false;
      
      String res0 = res_name + "\0" + res_class;
      return res.equals (res0);
    }
  
  
    public String res_name () {
      return res.substring (0, middle);
    }
  
  
    public String res_class () {
      return res.substring (middle+1, res.length ());
    }
  
  
    public String toString () {
      return "[" + res_name () + " " + res_class () + "]";
    }
  }


  /**
   * @see #set_wm_class_hint(String, String)
   */
  public void set_wm_class_hint (WMClassHint class_hint) {    
    set_wm_class_hint (class_hint.res_name (), class_hint.res_class ());
  }


  /**
   * @see <a href="XSetClassHint.html">XSetClassHint</a>
   * @see #change_property(int, int, Atom, Atom, int, Object, int, int)
   */
  public void set_wm_class_hint (String res_name, String res_class) {
    String wm_class = res_name + '\0' + res_class + '\0';

    change_property (REPLACE, Atom.WM_CLASS, Atom.STRING, 8,
                     wm_class.getBytes (), 0, 8);
  }


  /** X window manager hints. */
  public static class WMHints extends Data {
    public WMHints (Data data) { super (data); }
  
  
    public static final int INPUT_HINT_MASK = 1<<0;
    public static final int STATE_HINT_MASK = 1<<1;
    public static final int ICON_PIXMAP_HINT_MASK = 1<<2;
    public static final int ICON_WINDOW_HINT_MASK = 1<<3;
    public static final int ICON_POSITION_HINT_MASK = 1<<4;
    public static final int ICON_MASK_HINT_MASK = 1<<5;
    public static final int WINDOW_GROUP_HINT_MASK = 1<<6;
    public static final int URGENCY_HINT_MASK = 1<<8;
  
  
    /**
     * @return valid:
     * {@link #INPUT_HINT_MASK},
     * {@link #STATE_HINT_MASK},
     * {@link #ICON_PIXMAP_HINT_MASK},
     * {@link #ICON_WINDOW_HINT_MASK},
     * {@link #ICON_POSITION_HINT_MASK},
     * {@link #ICON_MASK_HINT_MASK},
     * {@link #WINDOW_GROUP_HINT_MASK},
     * {@link #URGENCY_HINT_MASK}
     */
    public int flags () { return read4 (32); }
  
  
    public final static int WITHDRAWN = 0;
    public final static int NORMAL = 1;
    public final static int ICONIC = 3;
  
  
    /**
     * @return valid:
     * {@link #WITHDRAWN},
     * {@link #NORMAL},
     * {@link #ICONIC}
     */
    public int initial_state () { return read4 (40); }
  }


  /**
   * @see <a href="XSetWMHints.html">XSetWMHints</a>
   * @see #change_property(int, int, Atom, Atom, int, Object, int, int)
   */
  public void set_wm_hints (WMHints wm_hints) {
    change_property (REPLACE, Atom.WM_HINTS, Atom.WM_HINTS, 8, wm_hints.data,
                     32, 8);
  }

   
  /** X window manager size hints. */
  public static class WMSizeHints extends Data {
    public WMSizeHints (Data data) { super (data); }
  
  
    // reading
  
    public final static int USPOSITION_MASK = 1<<0;
    public final static int USSIZE_MASK = 1<<1;
    public final static int PPOSITION_MASK = 1<<2;
    public final static int PSIZE_MASK = 1<<3;
    public final static int PMIN_SIZE_MASK = 1<<4;
    public final static int PMAX_SIZE_MASK = 1<<5;
    public final static int PRESIZE_INC_MASK = 1<<6;
    public final static int PASPECT_MASK = 1<<7;
    public final static int PBASE_SIZE_MASK = 1<<8;
    public final static int PWIN_GRAVITY_MASK = 1<<9;
  
  
    /**
     * @return valid: 
     * {@link #USPOSITION_MASK},
     * {@link #USSIZE_MASK},
     * {@link #PPOSITION_MASK},
     * {@link #PSIZE_MASK},
     * {@link #PMIN_SIZE_MASK},
     * {@link #PMAX_SIZE_MASK},
     * {@link #PRESIZE_INC_MASK},
     * {@link #PASPECT_MASK},
     * {@link #PBASE_SIZE_MASK},
     * {@link #PWIN_GRAVITY_MASK}
     */
    public int flags () { return read4 (32); }
  
  
    // skip 4 paddings in July 27, 1988 draft of icccm? 
    // but apps still use it, and how otherwise can we get these info?
    public boolean user_position () { return (flags () & USPOSITION_MASK) != 0; }
    public boolean user_size () { return (flags () & USSIZE_MASK) != 0; }
    public boolean program_position () { return (flags () & PPOSITION_MASK) != 0; }
    public boolean program_size () { return (flags () & PSIZE_MASK) != 0; }
  
    public int x () { return read4 (36); }
    public int y () { return read4 (40); }
    public int width () { return read4 (44); }
    public int height () { return read4 (48); }
    public int min_width () { return read4 (52); }
    public int min_height () { return read4 (56); }
  
  
    // writing
  
    public void x (int i) { write4 (36, i); }
  }


  /**
   * @see <a href="XSetWMNormalHints.html">XSetWMNormalHints</a>
   * @see #change_property(int, int, Atom, Atom, int, Object, int, int)
   */
  public void set_wm_normal_hints (WMSizeHints size_hints) {
    change_property (REPLACE, Atom.WM_NORMAL_HINTS, Atom.WM_SIZE_HINTS, 32,
                     size_hints.data, 32, 8);
  }


  /**
   * @see <a href="XSetWMName.html">XSetWMName</a>
   * @see #change_property(int, int, Atom, Atom, int, Object, int, int)
   */
  public void set_wm_name (String wm_name) {
    change_property (REPLACE, Atom.WM_NAME, Atom.STRING, 8,
                     wm_name.getBytes (), 0, 8); // support other types?
  }


  /** 
   * @see #set_wm_protocol(String)
   */
  public void set_wm_delete_window () {
    set_wm_protocol ("WM_DELETE_WINDOW");
  }


  /** 
   * @see #change_property(Atom, Atom, int)
   */
  public void set_wm_protocol (String name) {
    Atom wm_protocols = (Atom) Atom.intern (display, "WM_PROTOCOLS");
    Atom protocol =  (Atom) Atom.intern (display, name);

    change_property (wm_protocols, Atom.ATOM, protocol.id);
  }
  

  /** 
   * @see #set_wm_state(int, Window)
   */
  public void set_wm_state (int state) {
    set_wm_state (state, NONE);
  }


  /** X window manager state. */
  public static class WMState extends Data {
    public Display display;

    public WMState (Display display, Data data) { 
      super (data); 
      this.display = display;
    }
  
  
    public final static int WITHDRAWN = 0;
    public final static int NORMAL = 1;
    public final static int ICONIC = 3;
  
  
    /**
     * @return valid:
     * {@link #WITHDRAWN},
     * {@link #NORMAL},
     * {@link #ICONIC}
     */
    public int state () { return read4 (32); }
  
  
    public int icon_id () { return read4 (36); }
    public Window icon () { return (Window) intern (display, icon_id ()); }
  }
  
  
  /** 
   * @see #set_wm_state(int, Window)
   */
  public void set_wm_state (WMState state) {
    set_wm_state (state.state (), state.icon ());
  }


  /** 
   * @see #change_property(int, int, Atom, Atom, int, Object, int, int)
   */
  public void set_wm_state (int state, Window icon) {
    // FIXME: Re-think WM -stuff. Maybe do outside of Window as this is
    // not in the core protocol.

    Atom wm_state = (Atom) Atom.intern (display, "WM_STATE");
    int [] data = {state, icon.id};

    change_property (REPLACE, wm_state, wm_state, 32, data, 0, 32);
  }


  public String toString () {
    return "#Window " + id 
      + " " + (new Rectangle (x, y, width, height)).spec ();
  }

  
  public static final int MAX_WM_LENGTH = 1000;


  /**
   * @see <a href="XGetClassHint.html">XGetClassHint</a>
   * @see #property(boolean, Atom, Atom, int, int)
   */
  public WMClassHint wm_class_hint () {
    // FIXME: Re-think WM -stuff. Maybe do outside of Window as this is
    // not in the core protocol.
    return null;
//    PropertyReply pi = property (false, Atom.WM_CLASS, 
//      Atom.STRING, 0, MAX_WM_LENGTH); // support other types?
//
//    if (pi.format () != 8 || pi.type_id () != Atom.STRING.id)
//      return null;
//
//    return new WMClassHint (pi);
  }


  /**
   * @see <a href="XGetWMHints.html">XGetWMHints</a>
   * @see #property(boolean, Atom, Atom, int, int)
   */
  public WMHints wm_hints () {
    // FIXME: Re-think WM -stuff. Maybe do outside of Window as this is
    // not in the core protocol.
    return null;
//    PropertyReply pi = property (false, Atom.WM_HINTS, Atom.WM_HINTS, 0, 8);
//    
//    if (pi.format () != 32 || pi.type_id () != Atom.WM_HINTS.id
//      || pi.length () != 8) return null;
//
//    return new WMHints (pi);
  }


  /**
   * @see <a href="XGetWMName.html">XGetWMName</a>
   * @see #property(boolean, Atom, Atom, int, int)
   */
  public String wm_name () {
    Property pi = get_property (false, Atom.WM_NAME, Atom.STRING, 0,
                                MAX_WM_LENGTH); // support other types?

    if (pi.format () != 8 || pi.type_id () != Atom.STRING.id) 
      return null;

    return pi.string_value ();
  }


  /**
   * @see <a href="XGetWMNormalHints.html">XGetWMNormalHints</a>
   * @see #property(boolean, Atom, Atom, int, int)
   */
  public WMSizeHints wm_normal_hints () {
    // FIXME: Re-think WM -stuff. Maybe do outside of Window as this is
    // not in the core protocol.
    return null;
//    PropertyReply pi = property (false, Atom.WM_NORMAL_HINTS,
//      Atom.WM_SIZE_HINTS, 0, 18);
//
//    if (pi.format () != 32 
//      || pi.type_id () != Atom.WM_SIZE_HINTS.id
//      || pi.length () != 18) return null;
//   
//    return new WMSizeHints (pi);
  }


  /**
   * @see #wm_protocols()
   */
  public boolean wm_protocol (String name) {
    Atom protocol = (Atom) Atom.intern (display, name);
    int[] list = wm_protocols ();

    for (int i : list) {
      if (i == protocol.id)
        return true;
    }

    return false;
  }
      

  /** 
   * @return valid:
   * {@link Enum#next()} of type {@link Atom},
   * {@link Enum#next4()}
   *
   * @see #property(boolean, Atom, Atom, int, int)
   */
  public int[] wm_protocols () {
    // FIXME: Re-think WM -stuff. Maybe do outside of Window as this is
    // not in the core protocol.
    return new int[0];
//    Atom wm_protocols = (Atom) Atom.intern (display, "WM_PROTOCOLS");
//    PropertyReply pi = property (false, wm_protocols, Atom.ATOM, 0,
//      MAX_WM_LENGTH/4);
//
//    if (pi.byte_after () != 0)
//      throw new RuntimeException ("Number of WM protocol exceeds " +
//	MAX_WM_LENGTH/4); 
//
//    return new Enum (pi, 32, pi.length ()) {
//      public Object next () {
//        return Atom.intern (display, next4 ());
//      }
//    };
  }


  /** 
   * @see #property(boolean, Atom, Atom, int, int)
   */
  public WMState wm_state () {
    // FIXME: Re-think WM -stuff. Maybe do outside of Window as this is
    // not in the core protocol.
    return null;
//    Atom wm_state = (Atom) Atom.intern (display, "WM_STATE");    
//    PropertyReply pi = property (false, wm_state, wm_state, 0, 2);
//
//    if (pi.format () != 32 
//      || pi.type_id () != wm_state.id
//      || pi.length () != 2) return null;
//
//    return new WMState (display, pi);
  }


  /**
   * @see #warp_pointer(Window, int, int, int, int, int, int)
   */
  public void warp_pointer (int x, int y) {
    warp_pointer (NONE, 0, 0, 0, 0, x, y);
  }


  /**
   * @see #warp_pointer(int, int)
   */
  public void warp_pointer (Point position) {
    warp_pointer (position.x, position.y);
  }

  public int id () {
    return id;
  }

}
