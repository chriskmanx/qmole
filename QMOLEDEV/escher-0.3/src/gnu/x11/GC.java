package gnu.x11;


/**
 * X graphics context. This is used to change settings for drawing.
 */
public class GC extends Fontable {

  /**
   * Aggregates ChangeGC requests.
   */
  private class ChangeGCRequestObject implements RequestObject {

    /**
     * The values that are changes.
     */
    Values values;

    ChangeGCRequestObject () {
      values = new Values ();
    }

    void clear () {
      values.clear ();
    }

    /**
     * Creates a new ChangeGCRequestObject.
     *
     * @param v the values to be changed
     */
    ChangeGCRequestObject (Values v) {
      values = v;
    }

    /**
     * Writes this object to the connection stream.
     *
     * @param c the connection
     */
    public void write (RequestOutputStream s) {

      s.set_index (2);
      s.write_int16 (3 + values.count());
      s.write_int32 (GC.this.id);
      s.write_int32 (values.bitmask);
      values.write (s);
    }
    
  }

  private ChangeGCRequestObject changeGCRequest = new ChangeGCRequestObject ();

  public GC (Display display) {
    super (display);
  }

  public GC (GC src, int mask) {
    super (src.display);
    src.copy (this, mask);
  }

  public GC (GC src) {
    this (src, Values.ALL);
  }

  /** X GC values. */
  public static class Values extends ValueList {
    public static final Values EMPTY = new Values ();
    public Values () { super (23); }
  
    
    /** 0. */
    public static final int CLEAR = 0;
  
    /** src AND dst. */
    public static final int AND = 1;
  
    /** src AND NOT dst. */
    public static final int AND_REVERSE = 2;
  
    /** src. */
    public static final int COPY = 3;
    
    /** NOT src AND dst. */
    public static final int AND_INVERTED = 4;
  
    /** dst. */
    public static final int NOOP = 5;
  
    /** src XOR dst. */
    public static final int XOR = 6;
  
    /** src OR dst. */
    public static final int OR = 7;
  
    /** NOT src AND NOT dst. */
    public static final int NOR = 8;
  
    /** NOT src XOR dst. */
    public static final int EQUIV = 9;
  
    /** NOT dst. */
    public static final int INVERT = 10;
  
    /** src OR NOT dst. */
    public static final int OR_REVERSE = 11;
  
    /** NOT src. */
    public static final int COPY_INVERTED = 12;
  
    /** NOT src OR dst. */
    public static final int OR_INVERTED = 13;
  
    /** NOT src OR NOT dst. */
    public static final int NAND = 14;
  
    /** 1. */
    public static final int SET = 15;
  
  
    /**
     * @param i valid:
     * {@link #CLEAR},
     * {@link #AND},
     * {@link #AND_REVERSE},
     * {@link #COPY} (default),
     * {@link #AND_INVERTED},
     * {@link #NOOP},
     * {@link #XOR},
     * {@link #OR},
     * {@link #NOR},
     * {@link #EQUIV},
     * {@link #INVERT},
     * {@link #OR_REVERSE},
     * {@link #COPY_INVERTED},
     * {@link #OR_INVERTED},
     * {@link #NAND},
     * {@link #SET}
     */
    public void set_function (int i) { set (0, i); }
  
  
    /** 
     * @param i default: all ones
     */
    public void set_plane_mask (int i) { set (1, i); }
  
  
    /**
     * @see #set_foreground(int)
     */
    public void set_foreground (Color c) { set_foreground (c.pixel); }
  
  
    /** 
     * @param i default: 0
     */
    public void set_foreground (int pixel) { set (2, pixel); }
  
  
    /**
     * @see #set_background(int)
     */
    public void set_background (Color c) { set_background (c.pixel); }
  
  
    /** 
     * @param i default: 1
     */
    public void set_background (int pixel) { set (3, pixel); }
  
  
    /** 
     * @param i default: 0
     */
    public void set_line_width (int i) { set (4, i); }
  
  
    public static final int SOLID = 0;
    public static final int ON_OFF_DASH = 1;
    public static final int DOUBLE_DASH = 2;
  
  
    /** 
     * @param i valid:
     * {@link #SOLID} (default),
     * {@link #ON_OFF_DASH},
     * {@link #DOUBLE_DASH}
     **/
    public void set_line_style (int i) { set (5, i); }
  
  
    public static final int NOT_LAST = 0;
    public static final int BUTT = 1;
    public static final int CAP_ROUND = 2;
    public static final int PROJECTING = 3;
  
  
    /** 
     * @param i valid:
     * {@link #NOT_LAST} (default),
     * {@link #BUTT},
     * {@link #CAP_ROUND},
     * {@link #PROJECTING}
     */
    public void set_cap_style (int i) { set (6, i); }
  
  
    public static final int MITER = 0;
    public static final int JOIN_ROUND = 1;
    public static final int BEVEL = 2;
  
  
    /** 
     * @param i valid:
     * {@link #MITER} (default),
     * {@link #JOIN_ROUND},
     * {@link #BEVEL}
     */
    public void set_join_style (int i) { set (7, i); }
  
  
    public static final int TILED = 1;
    public static final int OPAQUE_STIPPLED = 2;
    public static final int STIPPLED = 3;
  
  
    /** 
     * @param i valid:
     * {@link #SOLID} (default),
     * {@link #TILED},
     * {@link #OPAQUE_STIPPLED},
     * {@link #STIPPLED}
     */
    public void set_fill_style (int i) { set (8, i); }
  
  
    public static final int EVEN_ODD = 0;
    public static final int WINDING = 1;
  
  
    /** 
     * @param i valid:
     * {@link #EVEN_ODD} (default),
     * {@link #WINDING}
     */
    public void set_fill_rule (int i) { set (9, i); }
  
  
    /** 
     * @param p default: pixmap of unspecified size filled with foreground
     * pixel
     */
    public void set_tile (Pixmap p) { set (10, p.id); }
  
  
    /** 
     * @param p default: pixmap of unspecified size filled with ones
     */
    public void set_stipple (Pixmap p) { set (11, p.id); }
  
  
    /** 
     * @param i default: 0
     */
    public void set_tile_stipple_x_origin (int i) { set (12, i); }
  
  
    /** 
     * @param i default: 0
     */
    public void set_tile_stipple_y_origin (int i) { set (13, i); }
  
  
    /** 
     * @param i default: server dependent font
     */
    public void set_font (Font f) { set (14, f.id); }
  
  
    public static final int CLIP_BY_CHILDREN = 0;
    public static final int INCLUDE_INTERIORS = 1;
  
  
    /** 
     * @param i valid:
     * {@link #CLIP_BY_CHILDREN} (default),
     * {@link #INCLUDE_INTERIORS}
     */
    public void set_subwindow_mode (int i) { set (15, i); }
  
  
    /** 
     * @param b default: true
     */
    public void set_graphics_exposures (boolean b) { set (16, b); }
  
  
    /** 
     * @param i default: 0
     */
    public void set_clip_x_origin (int i) { set (17, i); }
  
  
    /** 
     * @param i default: 0
     */
    public void set_clip_y_origin (int i) { set (18, i); }
  
  
    /**
     * @param p possible: {@link Pixmap#NONE} (default)
     */
    public void set_clip_mask (Pixmap p) { set (19, p.id); }
  
  
    /** 
     * @param i default: 0
     */
    public void set_dash_offset (int i) { set (20, i); }
  
  
    /** 
     * @param i default: 4 (that is, the list [4, 4])
     */
    public void set_dashes (int i) { set (21, i); }
  
  
    public static final int CHORD = 0;
    public static final int PIE_SLICE = 1;
  
  
    /** 
     * @param i valid:
     * {@link #CHORD} (default),
     * {@link #PIE_SLICE}
     */
    public void set_arc_mode (int i) { set (22, i); }
  
  
    public Object clone () {
      Values values = new Values ();
      values.copy (this);
      return values;
    }
  }


  public GC (Display display, Values values) {
    this (display.default_root, values);
  }


  /**
   * @see #create(Drawable, GC.Values)
   */
  public GC (Drawable drawable, Values values) {
    this (drawable.display);
    create (drawable, values);
  }


  /**
   * @see #GC(Drawable, GC.Values)
   */
  public GC (Drawable drawable) {
    this (drawable, Values.EMPTY);
  }


  // opcode 55 - create gc
  /**
   * Creates a GC for the specified drawable with the specified values.
   *
   * @see <a href="XCreateGC.html">XCreateGC</a>
   */
  public void create (Drawable drawable, Values values) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request(55, 0, 4 + values.count());
      o.write_int32 (id);
      o.write_int32 (drawable.id);
      o.write_int32 (values.bitmask);
      values.write (o);
      o.send ();
    }
  }

  // opcode 56 - change gc
  /**
   * Changes the current settings for this GC. This request will be aggregated.
   *
   * @param values the values to change
   *
   * @see <a href="XChangeGC.html">XChangeGC</a>
   * @see Request.Aggregate aggregation
   */
  public void change (Values values) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (56, 0, 0);
      ChangeGCRequestObject cr = new ChangeGCRequestObject (values);
      cr.write(o);
      o.send ();
    }
  }


  // opcode 57 - copy gc
  /**
   * Copies the state from this GC into the specified destination GC. The
   * mask is used to include/exclude specific state.
   *
   * @param dest the destination GC
   * @param mask the state mask
   *
   * @see <a href="XCopyGC.html">XCopyGC</a>
   */
  public void copy (GC dest, int mask) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (57, 0, 4);
      o.write_int32 (id); // Src-ID.
      o.write_int32 (dest.id); // Dst-ID.
      o.write_int32 (mask);
      o.send ();
    }

  }

  
  // opcode 58 - set dashes
  /**
   * Sets the dashes used for drawing lines.
   *
   * @param dash_offset the dash offset
   * @param dashes the dashes spec
   *
   * @see <a href="XSetDashes.html">XSetDashes</a>
   */
  public void set_dashes (int dash_offset, byte [] dashes) {
    
    int n = dashes.length;
    int p = 4 - (n % 4);

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (58, 0, 3 + (n + p) / 4);

      o.write_int32 (id); // The GC id.
      o.write_int16 (dash_offset); // The dash offset.
      o.write_int16 (n); // The number of dashes.
      o.write_bytes (dashes); // The actual dashes.

      for (int i = 0; i < p; i++)
        o.write_int8 (0); // Pad.

      o.send ();
    }
  }


  public static final int UN_SORTED = 0;
  public static final int Y_SORTED = 1;
  public static final int YX_SORTED = 2;
  public static final int YX_BANDED = 3;


  // opcode 59 - set clip rectangles
  /**
   * @param ordering valid:
   * {@link #UN_SORTED},
   * {@link #Y_SORTED},
   * {@link #YX_SORTED},
   * {@link #YX_BANDED}
   *
   * @see <a href="XSetClipRectangles.html">XSetClipRectangles</a>
   */
  public void set_clip_rectangles (int clip_x_origin, int clip_y_origin,
                                   Rectangle [] rectangles, int ordering) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (59, ordering, 3+2*rectangles.length);
      o.write_int32 (id);
      o.write_int16 (clip_x_origin);
      o.write_int16 (clip_y_origin);

      for (int i = 0; i < rectangles.length; i++) {
        o.write_int16 (rectangles [i].x);
        o.write_int16 (rectangles [i].y);
        o.write_int16 (rectangles [i].width);
        o.write_int16 (rectangles [i].height);
      }
      o.send ();
    }
  }


  // opcode 60 - free gc
  /**
   * @see <a href="XFreeGC.html">XFreeGC</a>
   */
  public void free () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (60, 0, 2);
      o.write_int32 (id);
      o.send ();
    }
  }


  /**
   * @see #GC(Drawable)
   */
  public static GC build (Display display) {
    return new GC (display.default_root);
  }


  /**
   * @see #create(GC.Values)
   */
  public void create () {
    create (Values.EMPTY);
  }


  /**
   * @see #create(Drawable, GC.Values)
   */
  public void create (Values values) {
    create (display.default_root, values);
  }


  /**
   * @see #copy(int)
   */
  public GC copy () {
    return copy (Values.ALL);
  }


  /**
   * @see #copy(GC, int)
   */
  public GC copy (int mask) {
    GC gc = build (display);
    copy (gc, mask);
    return gc;
  }

  
  /**
   * @see #change(GC.Values)
   * @see Values#set_arc_mode(int)
   */
  public void set_arc_mode (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_arc_mode (i);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_arc_mode (i);
        o.request_object = changeGCRequest;
      }
    }
  }    


  /**
   * @see #set_background(int)
   */
  public void set_background (Color c) {
    set_background (c.pixel);
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_background(int)
   */
  public void set_background (int pixel) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_background (pixel);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_background (pixel);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_cap_style(int)
   */
  public void set_cap_style (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_cap_style (i);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_cap_style (i);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_clip_mask(Pixmap)
   */
  public void set_clip_mask (Pixmap pixmap) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_clip_mask (pixmap);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_clip_mask (pixmap);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_clip_x_origin(int)
   */
  public void set_clip_x_origin (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_clip_x_origin (i);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_clip_x_origin (i);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_clip_y_origin(int)
   */
  public void set_clip_y_origin (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_clip_y_origin (i);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_clip_y_origin (i);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_dashes(int)
   */
  public void set_dashes (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_dashes (i);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_dashes (i);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_dash_offset(int)
   */
  public void set_dash_offset (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_dash_offset (i);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_dash_offset (i);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_fill_rule(int)
   */
  public void set_fill_rule (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_fill_rule (i);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_fill_rule (i);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_fill_style(int)
   */
  public void set_fill_style (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_fill_style (i);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_fill_style (i);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_font(Font)
   */
  public void set_font (Font font) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_font (font);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_font (font);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #set_foreground(int)
   */
  public void set_foreground (Color c) {
    set_foreground (c.pixel);
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_foreground(int)
   */
  public void set_foreground (int pixel) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_foreground (pixel);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_foreground (pixel);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_function(int)
   */
  public void set_function (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_function (i);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_function (i);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_graphics_exposures(boolean)
   */
  public void set_graphics_exposures (boolean b) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_graphics_exposures (b);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_graphics_exposures (b);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_join_style(int)
   */
  public void set_join_style (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_join_style (i);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_join_style (i);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_line_style(int)
   */
  public void set_line_style (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_line_style (i);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_line_style (i);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_line_width(int)
   */
  public void set_line_width (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_line_width (i);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_line_width (i);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_plane_mask(int)
   */
  public void set_plane_mask (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_plane_mask (i);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_plane_mask (i);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_stipple(Pixmap)
   */
  public void set_stipple (Pixmap pixmap) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_stipple (pixmap);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_stipple (pixmap);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_subwindow_mode(int)
   */
  public void set_subwindow_mode (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_subwindow_mode (i);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_subwindow_mode (i);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_tile(Pixmap)
   */
  public void set_tile (Pixmap pixmap) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_tile (pixmap);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_tile (pixmap);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_tile_stipple_x_origin(int)
   */
  public void set_tile_stipple_x_origin (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_tile_stipple_x_origin (i);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_tile_stipple_x_origin (i);
        o.request_object = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_tile_stipple_y_origin(int)
   */
  public void set_tile_stipple_y_origin (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.opcode () == 56
          && o.request_object instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.request_object;
        r.values.set_tile_stipple_x_origin (i);
      } else {
        o.begin_request (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_tile_stipple_x_origin (i);
        o.request_object = changeGCRequest;
      }
    }
  }
}
