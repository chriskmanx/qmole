package gnu.x11.extension;

import gnu.x11.Data;
import gnu.x11.Drawable;
import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;
import gnu.x11.Window;


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
  public static final String [] MINOR_OPCODE_STRINGS = {
    "GetVersion",               // 0
    "AllocateBackBufferName",   // 1
    "DeallocateBackBufferName", // 2
    "SwapBuffers",              // 3
    "BeginIdiom",               // 4
    "EndIdiom",                 // 5
    "GetVisualInfo"             // 6
  };
     

  public static final int UNDEFINED = 0;
  public static final int BACKGROUND = 1;
  public static final int UNTOUCHED = 2;
  public static final int COPIED = 3;

  public static final int CLIENT_MAJOR_VERSION = 1;
  public static final int CLIENT_MINOR_VERSION = 0;


  public int server_major_version, server_minor_version;


  // dbe opcode 0 - get version
  public DBE (gnu.x11.Display display) throws NotFoundException { 
    super (display, "DOUBLE-BUFFER", MINOR_OPCODE_STRINGS, 1, 0);

    // check version before any other operations
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 0, 2);
      o.write_int8 (CLIENT_MAJOR_VERSION);
      o.write_int8 (CLIENT_MINOR_VERSION);
      o.skip (2);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        server_major_version = i.read_int8 ();
        server_minor_version = i.read_int8 ();
        i.skip (22);
      }
    }
  }


  // dbe opcode 3 - swap buffers
  /**
   * @param actions array of actions; valid action: 
   * {@link #UNDEFINED}
   * {@link #BACKGROUND}
   * {@link #UNTOUCHED}
   * {@link #COPIED}
   *
   * @see <a href="XdbeSwapBuffers.html">XdbeSwapBuffers</a>
   */  
  public void swap (Window [] windows, int [] actions) {
    int n = windows.length;

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 3, 2 + 2 * n);
      o.write_int32 (n);
      for (int i = 0; i < n; i++) {
        o.write_int32 (windows [i].id);
        o.write_int8 (actions [i]);
        o.skip (3);
      }
      o.send ();
    }
  }


  // dbe opcode 4 - begin idiom
  /**
   * @see <a href="XdbeBeginIdiom.html">XdbeBeginIdiom</a>
   */
  public void begin_idiom () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 4, 1);
      o.send ();
    }
  }


  // dbe opcode 5 - end idiom
  /**
   * @see <a href="XdbeEndIdiom.html">XdbeEndIdiom</a>
   */
  public void end_idiom () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 5, 1);
      o.send ();
    }
  }


  /** DBE visual info. */
  public static class VisualInfo {

    public int visual_id;
    public int depth;
    public int perflevel;
    public VisualInfo (ResponseInputStream i) {
      visual_id = i.read_int32 ();
      depth = i.read_int8 ();
      perflevel = i.read_int8 ();
      i.skip (2);
    }

    public String toString () {
      return "#VisualInfo"
        + "\n  visual-id: " + visual_id
        + "\n  depth: " + depth
        + "\n  performance hint: " + perflevel;
    }
  }


  /** DBE screen visual info. */
  public static class ScreenVisualInfo {

    public VisualInfo [] infos;

    public ScreenVisualInfo (ResponseInputStream i) { 
      int number = i.read_int32 ();
      infos = new VisualInfo [number];
      for (int j = 0; j < number; j++) {
        infos [j] = new VisualInfo (i);
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
   * @param screen_specifiers valid: {@link #EMPTY}
   * @see <a href="XdbeGetVisualInfo.html">XdbeGetVisualInfo</a>
   */
  public ScreenVisualInfo [] visual_info (Drawable [] screen_specifiers) {

    ScreenVisualInfo[] infos;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 6, 2 + screen_specifiers.length);
      o.write_int32 (screen_specifiers.length);
      for (int i = 0; i < screen_specifiers.length; i++)
        o.write_int32 (screen_specifiers [i].id);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        int num = i.read_int32 ();
        i.skip (20);
        infos = new ScreenVisualInfo [num];
        for (int j = 0; j < num; j++) {
          infos [j] = new ScreenVisualInfo (i);
        }
      }
    }
    return infos;
  }


  public class BackBuffer extends Drawable {
    public Window window;


    // dbe opcode 1 - allocate back buffer name
    /**
     * @param swap_action_hint valid: 
     * {@link #UNDEFINED}
     * {@link #BACKGROUND}
     * {@link #UNTOUCHED}
     * {@link #COPIED}
     */
    public BackBuffer (Window window, int swap_action_hint) {
      super (window.display);
      this.window = window;
      width = window.width;
      height = window.height;

      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (major_opcode, 1, 4);
        o.write_int32 (window.id);
        o.write_int32 (id);
        o.write_int8 (swap_action_hint);
        o.skip (3);
        o.send ();
      }
    }


    // dbe opcode 2 - deallocate back buffer name
    /**
     * @see <a href="XdbeDeallocateBackBufferName.html">
     * XdbeDeallocateBackBufferName</a>
     */
    public void deallocate () {
      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (major_opcode, 2, 2);
        o.write_int32 (id);
        o.send ();
      }
    }


    // dbe opcode 7 - get back buffer attributes
    /**
     * @see <a href="XdbeGetBackBufferAttributes.html">
     * XdbeGetBackBufferAttributes</a>
     */
    public Window attributes () {

      int atts;
      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (major_opcode, 7, 2);
        o.write_int32 (id);
        ResponseInputStream i = display.in;
        synchronized (i) {
          i.read_reply (o);
          i.skip (8);
          atts = i.read_int32 ();
          i.skip (20);
        }
      }
      return (Window) Window.intern (this.display, atts);
    }


    /**
     * DBE#swap(Window, int)
     */
    public void swap (int action) {
      DBE.this.swap (window, action);
    }
  }


  // javadoc bug? should be BackBuffer#BackBuffer(Window, int)
  /**
   * @see <a href="XdbeAllocateBackBufferName.html">
   * XdbeAllocateBackBufferName</a>
   *
   * @see BackBuffer
   */
  public BackBuffer allocate (Window window, int swap_action_hint) {
    return new BackBuffer (window, swap_action_hint);
  }

  
  public static final String ERROR_STRING
    = "BAD_DBE_BUFFER: parameter not a DBE back buffer";


  public gnu.x11.Error build (gnu.x11.Display display, int code, int seq_no,
                              int bad, int minor_opcode, int major_opcode) {

    return new gnu.x11.Error (display, ERROR_STRING, code, seq_no, bad,
                              minor_opcode, major_opcode);
  }


  /** 
   * @see #swap(Window[], int[])
   */
  public void swap (Window window, int action) {
    swap (new Window [] {window}, new int [] {action});
  }


  public String more_string () {
    return "\n  client-version: " 
      + CLIENT_MAJOR_VERSION + "." + CLIENT_MINOR_VERSION
      + "\n  server-version: "
      + server_major_version + "." + server_minor_version;
  }
}
