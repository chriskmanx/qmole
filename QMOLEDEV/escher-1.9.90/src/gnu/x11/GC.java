package gnu.x11;

import gnu.x11.GC.Values.ArcMode;
import gnu.x11.GC.Values.CapStyle;
import gnu.x11.GC.Values.FillRule;
import gnu.x11.GC.Values.FillStyle;
import gnu.x11.GC.Values.FunctionValues;
import gnu.x11.GC.Values.JoinStyle;
import gnu.x11.GC.Values.LineStyle;
import gnu.x11.GC.Values.RectangleOrder;
import gnu.x11.GC.Values.SubWindowMode;


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

      s.setIndex (2);
      s.writeInt16 (3 + values.count());
      s.writeInt32 (GC.this.id);
      s.writeInt32 (values.bitmask);
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
  
    public enum FunctionValues {
        CLEAR(0),
        AND(1),
        AND_REVERSE(2),
        COPY(3),
        AND_INVERTED(4),
        NOOP(5),
        XOR(6),
        OR(7),
        NOR(8),
        EQUIV(9),
        INVERT(10),
        OR_REVERSE(11),
        COPY_INVERTED(12),
        OR_INVERTED(13),
        NAND(14),
        SET(15);

        private int code;
        
        FunctionValues(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }
    }
  
    public enum LineStyle {
        SOLID(0),
        DOUBLEDASH(1),
        ONOFFDASH(2);
        
        private int code;
        
        LineStyle(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }
    }
    
    public enum CapStyle {
        NOT_LAST(0),
        BUTT(1),
        CAP_ROUND(2),
        PROJECTING(3);
        
        private int code;
        
        CapStyle(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }
    }
    
    public enum JoinStyle {
        MITER(0),
        JOIN_ROUND(1),
        BEVEL(2);
        
        private int code;
        
        JoinStyle(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }
    }

    public enum FillStyle {
        TILED(0),
        OPAQUE_STIPPLED(1),
        STIPPLED(2);
        
        private int code;
        
        FillStyle(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }
    }
  
    public enum FillRule {
        EVEN_ODD(0),
        WINDING(1);
        
        private int code;
        
        FillRule(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }
    }
    
    
    public enum SubWindowMode {
        CLIP_BY_CHILDREN(0),
        INCLUDE_INTERIORS(1);
        
        private int code;
        
        SubWindowMode(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }
    }
    
    public enum ArcMode {
        CHORD(0),
        PIE_SLICE(1);
        
        private int code;
        
        ArcMode(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }
    }
    

    public enum RectangleOrder {
        UN_SORTED(0),
        Y_SORTED(1),
        YX_SORTED(2),
        YX_BANDED(3);
        
        private int code;
        
        RectangleOrder(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }   
    }

  
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
    public void setFunction (FunctionValues fv) { set (0, fv.getCode()); }
  
  
    /** 
     * @param i default: all ones
     */
    public void setPlaneMask (int i) { set (1, i); }
  
  
    /**
     * @see #setForeground(int)
     */
    public void setForeground (Color c) { setForeground (c.getPixel()); }
  
  
    /** 
     * @param i default: 0
     */
    public void setForeground (int pixel) { set (2, pixel); }
  
  
    /**
     * @see #setBackground(int)
     */
    public void setBackground (Color c) { setBackground (c.getPixel()); }
  
  
    /** 
     * @param i default: 1
     */
    public void setBackground (int pixel) { set (3, pixel); }
  
  
    /** 
     * @param i default: 0
     */
    public void setLineWidth (int i) { set (4, i); }
  
  
    /** 
     * @param i valid:
     * {@link #NOT_LAST} (default),
     * {@link #ON_OFF_DASH},
     * {@link #DOUBLE_DASH}
     **/
    public void setLineStyle (LineStyle ls) { set (5, ls.getCode()); }

  
    /** 
     * @param i valid:
     * {@link #NOT_LAST} (default),
     * {@link #BUTT},
     * {@link #CAP_ROUND},
     * {@link #PROJECTING}
     */
    public void setCapStyle (CapStyle cp) { set (6, cp.getCode()); }
  
  
    /** 
     * @param i valid:
     * {@link #MITER} (default),
     * {@link #JOIN_ROUND},
     * {@link #BEVEL}
     */
    public void setJoinStyle (JoinStyle js) { set (7, js.getCode()); }
  

    /** 
     * @param i valid:
     * {@link #NOT_LAST} (default),
     * {@link #TILED},
     * {@link #OPAQUE_STIPPLED},
     * {@link #STIPPLED}
     */
    public void setFillStyle (FillStyle fi) { set (8, fi.getCode()); }
 
  
    /** 
     * @param i valid:
     * {@link #EVEN_ODD} (default),
     * {@link #WINDING}
     */
    public void setFillRule (FillRule fr) { set (9, fr.getCode()); }
  
  
    /** 
     * @param p default: pixmap of unspecified size filled with foreground
     * pixel
     */
    public void setTile (Pixmap p) { set (10, p.id); }
  
  
    /** 
     * @param p default: pixmap of unspecified size filled with ones
     */
    public void setStipple (Pixmap p) { set (11, p.id); }
  
  
    /** 
     * @param i default: 0
     */
    public void setTileStippleXOrigin (int i) { set (12, i); }
  
  
    /** 
     * @param i default: 0
     */
    public void setTileStippleYOrigin (int i) { set (13, i); }
  
  
    /** 
     * @param i default: server dependent font
     */
    public void set_font (Font f) { set (14, f.id); }
  
  
    /** 
     * @param i valid:
     * {@link #CLIP_BY_CHILDREN} (default),
     * {@link #INCLUDE_INTERIORS}
     */
    public void setSubwindowMode (SubWindowMode swm) { set (15, swm.getCode()); }
  
  
    /** 
     * @param b default: true
     */
    public void setGraphicsExposures (boolean b) { set (16, b); }
  
  
    /** 
     * @param i default: 0
     */
    public void setClipXOrigin (int i) { set (17, i); }
  
  
    /** 
     * @param i default: 0
     */
    public void setClipYOrigin (int i) { set (18, i); }
  
  
    /**
     * @param p possible: {@link Pixmap#NONE} (default)
     */
    public void setClipMask (Pixmap p) { set (19, p.id); }
  
  
    /** 
     * @param i default: 0
     */
    public void setDashOffset (int i) { set (20, i); }
  
  
    /** 
     * @param i default: 4 (that is, the list [4, 4])
     */
    public void setDashes (int i) { set (21, i); }
  

    /** 
     * @param i valid:
     * {@link #CHORD} (default),
     * {@link #PIE_SLICE}
     */
    public void setArcMode (ArcMode am) { set (22, am.getCode()); }
  
  
    public Object clone () {
      Values values = new Values ();
      values.copy (this);
      return values;
    }
  }


  public GC (Display display, Values values) {
    this (display.getRootWindow(), values);
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

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest(55, 0, 4 + values.count());
      o.writeInt32 (id);
      o.writeInt32 (drawable.id);
      o.writeInt32 (values.bitmask);
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

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (56, 0, 0);
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

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (57, 0, 4);
      o.writeInt32 (id); // Src-ID.
      o.writeInt32 (dest.id); // Dst-ID.
      o.writeInt32 (mask);
      o.send ();
    }

  }

  
  // opcode 58 - set dashes
  /**
   * Sets the dashes used for drawing lines.
   *
   * @param dashOffset the dash offset
   * @param dashes the dashes spec
   *
   * @see <a href="XSetDashes.html">XSetDashes</a>
   */
  public void setDashes (int dashOffset, byte [] dashes) {
    
    int n = dashes.length;
    int p = 4 - (n % 4);

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (58, 0, 3 + (n + p) / 4);

      o.writeInt32 (id); // The GC id.
      o.writeInt16 (dashOffset); // The dash offset.
      o.writeInt16 (n); // The number of dashes.
      o.writeBytes (dashes); // The actual dashes.

      for (int i = 0; i < p; i++)
        o.writeInt8 (0); // Pad.

      o.send ();
    }
  }

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
  public void setClipRectangles (int clip_x_origin, int clip_y_origin,
                                   Rectangle [] rectangles,
                                   RectangleOrder ordering) {

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (59, ordering.getCode(), 3+2*rectangles.length);
      o.writeInt32 (id);
      o.writeInt16 (clip_x_origin);
      o.writeInt16 (clip_y_origin);

      for (Rectangle rec : rectangles) {
        o.writeInt16 (rec.getX());
        o.writeInt16 (rec.getY());
        o.writeInt16 (rec.getWidth());
        o.writeInt16 (rec.getHeight());
      }
      o.send ();
    }
  }


  // opcode 60 - free gc
  /**
   * @see <a href="XFreeGC.html">XFreeGC</a>
   */
  public void free () {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (60, 0, 2);
      o.writeInt32 (id);
      o.send ();
    }
  }


  /**
   * @see #GC(Drawable)
   */
  public static GC build (Display display) {
    return new GC (display.getRootWindow());
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
    create (display.getRootWindow(), values);
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
   * @see Values#setArcMode(ArcMode)
   */
  public void setArcMode (ArcMode am) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setArcMode (am);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setArcMode (am);
        o.requestObject = changeGCRequest;
      }
    }
  }    


  /**
   * @see #setBackground(int)
   */
  public void setBackground (Color c) {
    setBackground (c.getPixel());
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setBackground(int)
   */
  public void setBackground (int pixel) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setBackground (pixel);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setBackground (pixel);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setCapStyle(CapStyle)
   */
  public void setCapStyle (CapStyle cs) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setCapStyle (cs);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setCapStyle (cs);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setClipMask(Pixmap)
   */
  public void setClipMask (Pixmap pixmap) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setClipMask (pixmap);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setClipMask (pixmap);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setClipXOrigin(int)
   */
  public void setClipXOrigin (int i) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setClipXOrigin (i);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setClipXOrigin (i);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setClipYOrigin(int)
   */
  public void setClipYOrigin (int i) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setClipYOrigin (i);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setClipYOrigin (i);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setDashes(int)
   */
  public void setDashes (int i) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setDashes (i);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setDashes (i);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setDashOffset(int)
   */
  public void setDashOffset (int i) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setDashOffset (i);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setDashOffset (i);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setFillRule(int)
   */
  public void setFillRule (FillRule fr) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setFillRule (fr);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setFillRule (fr);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setFillStyle(int)
   */
  public void setFillStyle (FillStyle fs) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setFillStyle (fs);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setFillStyle (fs);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#set_font(Font)
   */
  public void setFont (Font font) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.set_font (font);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.set_font (font);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #setForeground(int)
   */
  public void setForeground (Color c) {
    setForeground (c.getPixel());
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setForeground(int)
   */
  public void setForeground (int pixel) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setForeground (pixel);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setForeground (pixel);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setFunction(int)
   */
  public void setFunction (FunctionValues fv) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setFunction (fv);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setFunction (fv);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setGraphicsExposures(boolean)
   */
  public void setGraphicsExposures (boolean b) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setGraphicsExposures (b);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setGraphicsExposures (b);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setJoinStyle(int)
   */
  public void setJoinSty (JoinStyle js) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setJoinStyle (js);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setJoinStyle (js);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setLineStyle(int)
   */
  public void setLineStyle (LineStyle ls) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setLineStyle (ls);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setLineStyle (ls);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setLineWidth(int)
   */
  public void setLineWidth (int i) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setLineWidth (i);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setLineWidth (i);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setPlaneMask(int)
   */
  public void setPlaneMask (int i) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setPlaneMask (i);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setPlaneMask (i);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setStipple(Pixmap)
   */
  public void setStipple (Pixmap pixmap) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setStipple (pixmap);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setStipple (pixmap);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setSubwindowMode(int)
   */
  public void setSubwindowMode (SubWindowMode swm) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setSubwindowMode (swm);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setSubwindowMode (swm);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setTile(Pixmap)
   */
  public void setTile (Pixmap pixmap) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setTile (pixmap);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setTile (pixmap);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setTileStippleXOrigin(int)
   */
  public void setTileStippleXOrigin (int i) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setTileStippleXOrigin (i);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setTileStippleXOrigin (i);
        o.requestObject = changeGCRequest;
      }
    }
  }


  /**
   * @see #change(GC.Values)
   * @see Values#setTileStippleYOrigin(int)
   */
  public void setTileStippleYOrigin (int i) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      if (o.opcode () == 56
          && o.requestObject instanceof ChangeGCRequestObject) {
        // Aggregate request.
        ChangeGCRequestObject r = (ChangeGCRequestObject) o.requestObject;
        r.values.setTileStippleXOrigin (i);
      } else {
        o.beginRequest (56, 0, 0);
        changeGCRequest.clear ();
        changeGCRequest.values.setTileStippleXOrigin (i);
        o.requestObject = changeGCRequest;
      }
    }
  }
}
