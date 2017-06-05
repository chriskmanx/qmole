package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X leave notify event. */
public final class LeaveNotify extends Input {
  public static final int CODE = 8;


  public LeaveNotify (Display display, ResponseInputStream in) {
    super (display, in);
  }
}
