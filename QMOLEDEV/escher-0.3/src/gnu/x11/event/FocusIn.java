package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X focus in event. */
public final class FocusIn extends FocusEvent {
  public static final int CODE = 9;

  public FocusIn (Display display, ResponseInputStream in) {
    super (display, in);
  }
}
