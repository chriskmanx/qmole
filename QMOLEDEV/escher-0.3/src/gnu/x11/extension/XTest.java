package gnu.x11.extension;

import gnu.x11.Cursor;
import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;
import gnu.x11.Window;


/** 
 * XTEST Extension. The specification can be found <a href= 
 * "http://escher.sourceforge.net/etc/specification/xtest-library.ps.gz"
 * >here</a> (<a href=
 * "http://escher.sourceforge.net/etc/specification/xtest-protocol.ps.gz"
 * >protocol</a>).
 */
public class XTest extends Extension {
  public static final String [] MINOR_OPCODE_STRINGS = {
    "GetVersion",               // 0
    "CompareCursor",            // 1
    "FakeInput",                // 2
    "GrabControl"               // 3
  };


  public static final int CLIENT_MAJOR_VERSION = 2;
  public static final int CLIENT_MINOR_VERSION = 1;


  public int server_major_version, server_minor_version;


  // xtest opcode 0 - get version
  /**
   * @see <a href="XTestQueryExtension.html">XTestQueryExtension</a>
   */
  public XTest (gnu.x11.Display display) throws NotFoundException { 
    super (display, "XTEST", MINOR_OPCODE_STRINGS); 

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 0, 2);
      o.write_int8 (CLIENT_MAJOR_VERSION);
      o.skip (1);
      o.write_int16 (CLIENT_MINOR_VERSION);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        server_major_version = i.read_int8 ();
        i.skip (6);
        server_minor_version = i.read_int16 ();
        i.skip (22);
      }
    }
  }


  // xtest opcode 1 - compare cursor
  /**
   * @param cursor possible:
   * {@link Cursor#NONE},
   * {@link Cursor#CURRENT}
   *
   * @see <a href="XTestCompareCursorWithWindow.html">
   * XTestCompareCursorWithWindow</a>
   */
  public boolean compare_cursor (Window window, Cursor cursor) {

    boolean same;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 1, 3);
      o.write_int32 (window.id);
      o.write_int32 (cursor.id);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        same = i.read_bool ();
        i.skip (30);
      }
    }
    return same;
  }


  public static final int KEY_PRESS = 2;
  public static final int KEY_RELEASE = 3;
  public static final int BUTTON_PRESS = 4;
  public static final int BUTTON_RELEASE = 5;
  public static final int MOTION_NOTIFY = 6;


  // xtest opcode 2 - fake input
  /**
   * @param type valid:
   * {@link #KEY_PRESS},
   * {@link #KEY_RELEASE},
   * {@link #BUTTON_PRESS},
   * {@link #BUTTON_RELEASE},
   * {@link #MOTION_NOTIFY}
   *
   * @param time possible: {@link gnu.x11.Display#CURRENT_TIME}
   */
  public void fake_input (int type, int detail, int delay, Window root, 
                          int x, int y) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 2, 9);
      o.write_int8 (type);
      o.write_int8 (detail);
      o.skip (2);
      o.write_int32 (delay);
      o.write_int32 (root.id);
      o.skip (8);
      o.write_int16 (x);
      o.write_int16 (y);
      o.send ();
    }
  }


  // xtest opcode 3 - grab control
  /**
   * @see <a href="XTestGrabControl.html">XTestGrabControl</a>
   */
  public void grab_control (boolean impervious) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 3, 2);
      o.write_bool (impervious);
      o.send ();
    }
  }


  /**
   * @see <a href="XTestFakeButtonEvent.html">XTestFakeButtonEvent</a>
   */
  public void fake_button_event (int button, boolean press, int delay) {
    fake_input (press ? BUTTON_PRESS : BUTTON_RELEASE, button, delay,
      Window.NONE, 0, 0);
  }

    
  /**
   * @see <a href="XTestFakeKeyEvent.html">XTestFakeKeyEvent</a>
   */
  public void fake_key_event (int keycode, boolean press, int delay) {
    fake_input (press ? KEY_PRESS : KEY_RELEASE, keycode, delay, 
      Window.NONE, 0, 0);
  }


  /**
   * @see <a href="XTestFakeMotionEvent.html">XTestFakeMotionEvent</a>
   */
  public void fake_motion_event (Window root, int x, int y, 
    boolean relative, int delay) {

    fake_input (MOTION_NOTIFY, relative ? 1 : 0, delay, root, x, y);
  }


  public String more_string () {
    return "\n  client-version: " 
      + CLIENT_MAJOR_VERSION + "." + CLIENT_MINOR_VERSION
      + "\n  server-version: "
      + server_major_version + "." + server_minor_version;
  }
}
