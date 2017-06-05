package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X visibility notify event. */
public final class VisibilityNotify extends Event {
  public static final int CODE = 15;

  public static final int UNOBSCURED = 0;
  public static final int PARTIALLY_UNOBSCURED = 1;
  public static final int FULLY_UNOBSCURED = 2;
  
  public int window_id;
  public int state;
  
  public VisibilityNotify (Display display, ResponseInputStream in) {
    super (display, in);
    window_id = in.read_int32 ();
    state = in.read_int8 ();
    in.skip (23);
  }
}
