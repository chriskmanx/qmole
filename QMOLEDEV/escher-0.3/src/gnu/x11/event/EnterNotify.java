package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X enter notify event. */
public final class EnterNotify extends Input {
  public static final int CODE = 7;


  public EnterNotify (Display display, ResponseInputStream in) {
    super (display, in);
  }
}
