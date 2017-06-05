package gnu.x11.extension;

import gnu.x11.Drawable;
import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;
import gnu.x11.Window;
import gnu.x11.Error.ErrorCode;


/** 
 * Double Buffer Extension. The specification can be found <a href= 
 * "http://escher.sourceforge.net/etc/specification/dbe-library.ps.gz"
 * >here</a> (<a href=
 * "http://escher.sourceforge.net/etc/specification/dbe-protocol.ps.gz"
 * >protocol</a>).
 * 
 * <p>Check <a href=" "../../../../etc/faq/dbe">this</a> for an insightful
 * discusson on the merit of DBE. 
 */
public class DBE extends Extension implements ErrorFactory {
    
  private static final String [] MINOR_OPCODE_STRINGS = {
    "GetVersion",               // 0
    "AllocateBackBufferName",   // 1
    "DeallocateBackBufferName", // 2
    "SwapBuffers",              // 3
    "BeginIdiom",               // 4
    "EndIdiom",                 // 5
    "GetVisualInfo"             // 6
  };
  
  public static final int CLIENT_MAJOR_VERSION = 1;
  public static final int CLIENT_MINOR_VERSION = 0;

  private int serverMajorVersion, serverMinorVersion;
     
  public enum SwapAction {
      UNDEFINED(0),
      BACKGROUND(1),
      UNTOUCHED(2),
      COPIED(3);
      
      private int code;
      
      SwapAction(int action) {
          this.code = action;
      }
      
      public int getCode() {
        return code;
      }
  }


  // dbe opcode 0 - get version
  public DBE(gnu.x11.Display display) throws NotFoundException { 
    super(display, "DOUBLE-BUFFER", MINOR_OPCODE_STRINGS, 1, 0);

    // check version before any other operations
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 0, 2);
      o.writeInt8(CLIENT_MAJOR_VERSION);
      o.writeInt8(CLIENT_MINOR_VERSION);
      o.skip(2);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.readReply(o);
        i.skip(8);
        serverMajorVersion = i.readInt8();
        serverMinorVersion = i.readInt8();
        i.skip(22);
      }
    }
  }


  // dbe opcode 3 - swap buffers
  /**
   * @param actions array of actions; valid action: 
   * {@link SwapAction.UNDEFINED}
   * {@link SwapAction.BACKGROUND}
   * {@link SwapAction.UNTOUCHED}
   * {@link SwapAction.COPIED}
   *
   * @see <a href="XdbeSwapBuffers.html">XdbeSwapBuffers</a>
   */  
  public void swap(Window [] windows, SwapAction[] actions) {
    int n = windows.length;

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 3, 2 + 2 * n);
      o.writeInt32(n);
      for (int i = 0; i < n; i++) {
        o.writeInt32(windows[i].getID());
        o.writeInt8(actions[i].getCode());
        o.skip(3);
      }
      o.send();
    }
  }


  // dbe opcode 4 - begin idiom
  /**
   * @see <a href="XdbeBeginIdiom.html">XdbeBeginIdiom</a>
   */
  public void beginIdiom() {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 4, 1);
      o.send();
    }
  }


  // dbe opcode 5 - end idiom
  /**
   * @see <a href="XdbeEndIdiom.html">XdbeEndIdiom</a>
   */
  public void endIdiom() {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 5, 1);
      o.send();
    }
  }


  /** DBE visual info. */
  public static class XdbeScreenVisualInfo {

    private int visualID;
    private int depth;
    private int perflevel;
    public XdbeScreenVisualInfo(ResponseInputStream i) {
      visualID = i.readInt32();
      depth = i.readInt8();
      perflevel = i.readInt8();
      i.skip(2);
    }

    public String toString() {
      return "#XdbeScreenVisualInfo"
        + "\n  visual-id: " + visualID
        + "\n  depth: " + depth
        + "\n  performance hint: " + perflevel;
    }
  }


  /** DBE screen visual info. */
  public static class ScreenVisualInfo {

    public XdbeScreenVisualInfo [] infos;

    public ScreenVisualInfo(ResponseInputStream i) { 
      int number = i.readInt32();
      infos = new XdbeScreenVisualInfo [number];
      for(int j = 0; j < number; j++) {
        infos [j] = new XdbeScreenVisualInfo(i);
      }
    }

  }


  public static final Drawable [] EMPTY = new Drawable [0];


  // dbe opcode 6 - get visual info
  /**
   * Get information about which visuals support double buffering. There
   * seems to be <a href= "../../../../etc/dbe-bug">bug</a> in all servers
   * based on X Consortium sample implementation up to R6.5 (including
   * XFree86 4.0.1 or earlier): the reply length of the reply is incorrect
   * , causing a "read timed out" error.
   *
   * @param screenSpecifiers valid: {@link #EMPTY}
   * @see <a href="XdbeGetVisualInfo.html">XdbeGetVisualInfo</a>
   */
  public ScreenVisualInfo [] visualInfo(Drawable [] screenSpecifiers) {

    ScreenVisualInfo[] infos;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 6, 2 + screenSpecifiers.length);
      o.writeInt32(screenSpecifiers.length);
      for (Drawable screen : screenSpecifiers)
        o.writeInt32(screen.getID());
      
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.readReply(o);
        i.skip(8);
        int num = i.readInt32();
        i.skip(20);
        infos = new ScreenVisualInfo [num];
        for (ScreenVisualInfo screen : infos)
          screen = new ScreenVisualInfo(i);
      }
    }
    return infos;
  }


  /** BackBuffer - The core of this extension */
  public class BackBuffer extends Drawable {
    private Window window;


    // dbe opcode 1 - allocate back buffer name
    /**
     * @param swapActionHint valid: 
     * {@link SwapAction.UNDEFINED}
     * {@link SwapAction.BACKGROUND}
     * {@link SwapAction.UNTOUCHED}
     * {@link SwapAction.COPIED}
     */
    public BackBuffer(Window window, SwapAction swapActionHint) {
      super(window.getDisplay());
      this.window = window;
      width = window.width;
      height = window.height;

      RequestOutputStream o = display.getResponseOutputStream();
      synchronized(o) {
        o.beginRequest(majorOpcode, 1, 4);
        o.writeInt32(window.getID());
        o.writeInt32(id);
        o.writeInt8(swapActionHint.getCode());
        o.skip(3);
        o.send();
      }
    }


    // dbe opcode 2 - deallocate back buffer name
    /**
     * @see <a href="XdbeDeallocateBackBufferName.html">
     * XdbeDeallocateBackBufferName</a>
     */
    public void deallocate() {
      RequestOutputStream o = display.getResponseOutputStream();
      synchronized(o) {
        o.beginRequest(majorOpcode, 2, 2);
        o.writeInt32(id);
        o.send();
      }
    }


    // dbe opcode 7 - get back buffer attributes
    /**
     * @see <a href="XdbeGetBackBufferAttributes.html">
     * XdbeGetBackBufferAttributes</a>
     */
    public Window attributes() {

      int atts;
      RequestOutputStream o = display.getResponseOutputStream();
      synchronized(o) {
        o.beginRequest(majorOpcode, 7, 2);
        o.writeInt32(id);
        ResponseInputStream i = display.getResponseInputStream();
        synchronized(i) {
          i.readReply(o);
          i.skip(8);
          atts = i.readInt32();
          i.skip(20);
        }
      }
      return (Window) Window.intern(this.display, atts);
    }


    /**
     * DBE#swap(Window, int)
     */
    public void swap(SwapAction action) {
      DBE.this.swap(window, new SwapAction[]{action});
    }
  }


  // javadoc bug? should be BackBuffer#BackBuffer(Window, int)
  /**
   * @see <a href="XdbeAllocateBackBufferName.html">
   * XdbeAllocateBackBufferName</a>
   *
   * @see BackBuffer
   */
  public BackBuffer allocate(Window window, SwapAction swapActionHint) {
    return new BackBuffer(window, swapActionHint);
  }

  
  public static final String ERROR_STRING
    = "BAD_DBE_BUFFER: parameter not a DBE back buffer";


  public gnu.x11.Error build(gnu.x11.Display display, int code, int seqNumber,
                              int bad, int minorOpcode, int majorOpcode) {
    ErrorCode errorCd = ErrorCode.getError(code);
    return new gnu.x11.Error(display, ERROR_STRING, errorCd, seqNumber, bad,
                              minorOpcode, majorOpcode);
  }


  /** 
   * @see #swap(Window[], int[])
   */
  public void swap(Window window, SwapAction[] action) {
    swap(new Window [] {window}, action);
  }


  public String moreString() {
    return "\n  client-version: " 
      + CLIENT_MAJOR_VERSION + "." + CLIENT_MINOR_VERSION
      + "\n  server-version: "
      + serverMajorVersion + "." + serverMinorVersion;
  }
}
