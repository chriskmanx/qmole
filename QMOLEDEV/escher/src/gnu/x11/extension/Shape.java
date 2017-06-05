package gnu.x11.extension;

import gnu.x11.Display;
import gnu.x11.Pixmap;
import gnu.x11.Rectangle;
import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;
import gnu.x11.Window;
import gnu.x11.event.Event;

/** 
 * Nonrectangular Window Extension. The specification can be found <a href= 
 * "http://escher.sourceforge.net/etc/specification/shape-library.ps.gz"
 * >here</a> (<a href=
 * "http://escher.sourceforge.net/etc/specification/shape-protocol.ps.gz"
 * >protocol</a>).
 */
public class Shape extends Extension implements EventFactory {
  private static final String[] MINOR_OPCODE_STRINGS = {
    "QueryVersion",             // 0
    "Rectangles",               // 1
    "Mask",                     // 2
    "Combine",                  // 3
    "Offset",                   // 4
    "QueryExtents",             // 5
    "SelectInput",              // 6
    "InputSelected",            // 7
    "GetRectangles"             // 8
  };
    

  public static final int CLIENT_MAJOR_VERSION = 1;
  public static final int CLIENT_MINOR_VERSION = 0;


  private int serverMajorVersion, serverMinorVersion;


  // shape opcode 0 - query version
  /**
   * @see <a href="XShapeQueryExtension.html">XShapeQueryExtension</a>
   */
  public Shape(Display display) throws NotFoundException { 
    super(display, "SHAPE", MINOR_OPCODE_STRINGS, 0, 1);

    // check version before any other operations
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 0, 1);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.readReply(o);
        i.skip(8);
        serverMajorVersion = i.readInt16();
        serverMinorVersion = i.readInt16();
        i.skip(20);
      }
    }
  }

  public enum Kind {
      BOUNDING(0),
      CLIP(1);
      
      private int code;
      
      Kind(int code) {
          this.code = code;
      }
      
      public int getCode() {
          return this.code;
      }
      
      public static Kind getByCode(int code) {
          return code == 0 ? BOUNDING : CLIP;
      }
  }

  public enum Operation {
      SET(0),
      UNION(1),
      INTERSECT(2),
      SUBTRACT(3),
      INVERT(4);
      
      private int code;
      
      Operation(int code) {
          this.code = code;
      }
      
      public int getCode() {
          return this.code;
      }
  }

  public enum Ordering {
      UN_SORTED(0),
      Y_SORTED(1),
      YX_SORTED(2),
      YX_BANDED(3);
      
      private int code;
      
      Ordering(int code) {
          this.code = code;
      }
      
      public int getCode() {
          return this.code;
      }
      
      public static Ordering getByCode(int code) {
          switch (code) {
              case 0: return UN_SORTED;
              case 1: return Y_SORTED;
              case 2: return YX_SORTED;
              case 3: return YX_BANDED;
              default: return UN_SORTED;
          }
      }
  }

  // shape opcode 1 - rectangles
  /**
   * @param destKind valid:
   * {@link Kind}
   * 
   * @param operation valid:
   * {@link Operation}
   * 
   * @param ordering valid: 
   * {@link Ordering}
   * 
   * @see <a href="XShapeCombineRectangles.html">XShapeCombineRectangles</a>
   */
  public void combineRectangles(Window dest, Kind destKind, int xOffset,
                                  int yOffset, Rectangle[] rectangles,
                                  Operation operation, Ordering ordering) {

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 1, 4 + 2 * rectangles.length);
      o.writeInt8(operation.getCode());
      o.writeInt8(destKind.getCode());
      o.writeInt8(ordering.getCode());
      o.skip(1);
      o.writeInt32(dest.getID());
      o.writeInt16(xOffset);
      o.writeInt16(yOffset);

      for (Rectangle rect : rectangles) {
        o.writeInt16(rect.getX());
        o.writeInt16(rect.getY());
        o.writeInt16(rect.getWidth());
        o.writeInt16(rect.getHeight());
      }

      o.send();
    }
  }


  // shape opcode 2 - mask
  /**
   * @param destKind valid:
   * {@link Kind},
   * 
   * @param operation valid:
   * {@link Operation}
   * 
   * @param src possible: {@link gnu.x11.Pixmap#NONE}
   * @see <a href="XShapeCombineMask.html">XShapeCombineMask</a>
   */
  public void combineMask(Window dest, Kind destKind, int xOffset, 
                            int yOffset, Pixmap src, Operation operation) { 

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 2, 5);
      o.writeInt8(operation.getCode());
      o.writeInt8(destKind.getCode());
      o.skip(2);
      o.writeInt32(dest.getID());
      o.writeInt16(xOffset);
      o.writeInt16(yOffset);
      o.writeInt32(src.getID());
      o.send();
    }
  }


  // shape opcode 3 - combine
  /**
   * @param destKind valid:
   * {@link Kind}
   * 
   * @param srcKind valid: 
   * {@link Kind}
   * 
   * @param operation valid:
   * {@link Operation}
   * 
   * @see <a href="XShapeCombineShape.html">XShapeCombineShape</a>
   */
  public void combineShape(Window dest, Kind destKind, int xOffset, int yOffset,
                           Window src, Kind srcKind, Operation operation) { 

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 3, 5);
      o.writeInt8(operation.getCode());
      o.writeInt8(destKind.getCode());
      o.writeInt8(srcKind.getCode());
      o.skip(1);
      o.writeInt32(dest.getID());
      o.writeInt16(xOffset);
      o.writeInt16(yOffset);
      o.writeInt32(src.getID());
      o.send();
    }
  }


  // shape opcode 4 - offset
  /**
   * @see <a href="XShapeOffsetShape.html">XShapeOffsetShape</a>
   */
  public void offsetShape(Window dest, Kind destKind, int xOffset, int yOffset) {

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 4, 4);
      o.writeInt8(destKind.getCode());
      o.skip(3);
      o.writeInt32(dest.getID());
      o.writeInt16(xOffset);
      o.writeInt16(yOffset);
      o.send();
    }
  }


  /** Reply of {@link #extents(Window)} */
  public static class ExtentsInfo {

    private boolean boundingShaped;
    private boolean clipShaped;
    private int boundingX;
    private int boundingY;
    private int boundingWidth;
    private int boundingHeight;
    private int clipX;
    private int clipY;
    private int clipWidth;
    private int clipHeight;
    
    ExtentsInfo(ResponseInputStream i) {
      boundingShaped = i.readBool();
      clipShaped = i.readBool();
      i.skip(2);
      boundingX = i.readInt16();
      boundingY = i.readInt16();
      boundingWidth = i.readInt16();
      boundingHeight = i.readInt16();
      clipX = i.readInt16();
      clipY = i.readInt16();
      clipWidth = i.readInt16();
      clipHeight = i.readInt16();
    }
  
    public Rectangle bounding() { 
      return new Rectangle(boundingX, boundingY, boundingWidth, boundingHeight);
    }
        
  
    public Rectangle clip() { 
      return new Rectangle(clipX, clipY, clipWidth, clipHeight);
    }
        
  
    public String toString() {
      String output = "#Extents ";
  
      if (boundingShaped) output += "Bounding: " + bounding();
      if (clipShaped) output += "Clip: " + clip();
  
      return output;
    }
  }
  
  
  // shape opcode 5 - query extents
  /**
   * @see <a href="XShapeQueryExtents.html">XShapeQueryExtents</a>
   */
  public ExtentsInfo extents(Window dest) {
    
    ExtentsInfo info;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 5, 2);
      o.writeInt32(dest.getID());
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.readReply(o);
        i.skip(8);
        info = new ExtentsInfo(i);
        i.skip(4);
      }
    }
    return info;
  }


  // shape opcode 6 - select input
  /**
   * @see <a href="XShapeSelectInput.html">XShapeSelectInput</a>
   */
  public void selectInput(Window dest, boolean enable) {

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 6, 3);
      o.writeInt32(dest.getID());
      o.writeBool(enable);
      o.send();    
    }
  }


  // shape opcode 7 - input selected
  /**
   * @see <a href="XShapeInputSelected.html">XShapeInputSelected</a>
   */
  public boolean input_selected(Window dest) {
    boolean enabled;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 6, 2);
      o.writeInt32(dest.getID());
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.readReply(o);
        i.skip(1);
        enabled = i.readBool();
        i.skip(24);
      }
    }
    return enabled;
  }

  
  /** Reply of {@link #rectangles()} */
  public static class RectanglesInfo {

    /**
     * One of {@link Ordering}.
     */
    private Ordering ordering;

    private Rectangle [] rectangles;

    RectanglesInfo(ResponseInputStream i) {
      ordering = Ordering.getByCode(i.readInt8());
      i.skip(6);
      int nrects = i.readInt32();
      rectangles = new Rectangle [nrects];
      i.skip(20);
      
      for (Rectangle rec : rectangles) {
        int x = i.readInt16();
        int y = i.readInt16();
        int w = i.readInt16();
        int h = i.readInt16();
        rec = new Rectangle(x, y, w, h);
      }
    }

  }
  
  
  // shape opcode 8 - get rectangles
  /**
   * @see <a href="XShapeGetRectangles.html">XShapeGetRectangles</a>
   */
  public RectanglesInfo rectangles(Window window, Kind kind) {

    RectanglesInfo info;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 7, 3);
      o.writeInt32(window.getID());
      o.writeInt8(kind.getCode());
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.readReply(o);
        i.skip(1);
        info = new RectanglesInfo(i);
      }
    }
    return info;
  }


  /** SHAPE notify event. */
  public static class NotifyEvent extends Event {
    public static final int code = 0;

    private int windowID;
    private int x;
    private int y;
    private int width;
    private int height;
    private int time;
    private boolean shaped;

    NotifyEvent(Display display, ResponseInputStream i) { 
      super(display, i);
      windowID = i.readInt32();
      x = i.readInt16();
      y = i.readInt16();
      width = i.readInt16();
      height = i.readInt16();
      time = i.readInt32();
      shaped = i.readBool();
      i.skip(11);
    }

    public int x() { return x; }
    public int y() { return y; }
    public int width() { return width; }
    public int height() { return height; }
    public int time() { return time; }
    public boolean shaped() { return shaped; }

    /**
     * @return valid:
     * {@link Kind}
     */
    public Kind kind() { return Kind.getByCode(detail); }


    public Rectangle rectangle() {
      return new Rectangle(x(), y(), width(), height());
    }
  }


  public Event build(Display display, ResponseInputStream i, int code) {
    // only one extension event
    return new NotifyEvent(display, i);
  }


  public String moreString() {
    return "\n  client-version: " 
      + CLIENT_MAJOR_VERSION + "." + CLIENT_MINOR_VERSION
      + "\n  server-version: "
      + serverMajorVersion + "." + serverMinorVersion;
  }
}
