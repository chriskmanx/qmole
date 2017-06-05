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
  private static final String [] MINOR_OPCODE_STRINGS = {
    "GetVersion",               // 0
    "CompareCursor",            // 1
    "FakeInput",                // 2
    "GrabControl"               // 3
  };


  public static final int CLIENT_MAJOR_VERSION = 2;
  public static final int CLIENT_MINOR_VERSION = 1;


  private int serverMajorVersion, serverMinorVersion;


  // xtest opcode 0 - get version
  /**
   * @see <a href="XTestQueryExtension.html">XTestQueryExtension</a>
   */
  public XTest(gnu.x11.Display display) throws NotFoundException { 
    super(display, "XTEST", MINOR_OPCODE_STRINGS); 

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 0, 2);
      o.writeInt8(CLIENT_MAJOR_VERSION);
      o.skip(1);
      o.writeInt16(CLIENT_MINOR_VERSION);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.readReply(o);
        i.skip(1);
        serverMajorVersion = i.readInt8();
        i.skip(6);
        serverMinorVersion = i.readInt16();
        i.skip(22);
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
  public boolean compareCursor(Window window, Cursor cursor) {

    boolean same;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 1, 3);
      o.writeInt32(window.getID());
      o.writeInt32(cursor.getID());
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.readReply(o);
        i.skip(1);
        same = i.readBool();
        i.skip(30);
      }
    }
    return same;
  }
  
  /**
   * Fake Event Types
   */
  public enum FakeEvent {
      KEY_PRESS(2),
      KEY_RELEASE(3),
      BUTTON_PRESS(4),
      BUTTON_RELEASE(5),
      MOTION_NOTIFY(6);
      
      private int code;
      
      FakeEvent(int code) {
          this.code = code;
      }
      
      public int getCode() {
          return this.code;
      }
  }


  // xtest opcode 2 - fake input
  /**
   * @param type valid {@link FakeEvent}
   * @param time possible: {@link gnu.x11.Display#CURRENT_TIME}
   */
  public void fakeInput(FakeEvent type, int detail, int delay, Window root, 
                          int x, int y) {

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 2, 9);
      o.writeInt8(type.getCode());
      o.writeInt8(detail);
      o.skip(2);
      o.writeInt32(delay);
      o.writeInt32(root.getID());
      o.skip(8);
      o.writeInt16(x);
      o.writeInt16(y);
      o.send();
    }
  }


  // xtest opcode 3 - grab control
  /**
   * @see <a href="XTestGrabControl.html">XTestGrabControl</a>
   */
  public void grabControl(boolean impervious) {

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 3, 2);
      o.writeBool(impervious);
      o.send();
    }
  }


  /**
   * @see <a href="XTestFakeButtonEvent.html">XTestFakeButtonEvent</a>
   */
  public void fake_button_event(int button, boolean press, int delay) {
    fakeInput(press ? FakeEvent.BUTTON_PRESS : FakeEvent.BUTTON_RELEASE,
                    button, delay, Window.NONE, 0, 0);
  }

    
  /**
   * @see <a href="XTestFakeKeyEvent.html">XTestFakeKeyEvent</a>
   */
  public void fakeKeyEvent(int keycode, boolean press, int delay) {
    fakeInput(press ? FakeEvent.KEY_PRESS : FakeEvent.KEY_RELEASE, keycode,
                    delay, Window.NONE, 0, 0);
  }


  /**
   * @see <a href="XTestFakeMotionEvent.html">XTestFakeMotionEvent</a>
   */
  public void fakeMotionEvent(Window root, int x, int y, 
    boolean relative, int delay) {

    fakeInput(FakeEvent.MOTION_NOTIFY, relative ? 1 : 0, delay, root, x, y);
  }


  public String moreString() {
    return "\n  client-version: " 
      + CLIENT_MAJOR_VERSION + "." + CLIENT_MINOR_VERSION
      + "\n  server-version: "
      + serverMajorVersion + "." + serverMinorVersion;
  }
}
