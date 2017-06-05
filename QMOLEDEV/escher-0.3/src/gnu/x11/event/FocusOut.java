package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X focus out event. */
public final class FocusOut extends FocusEvent {
  public static final int CODE = 10;


  public FocusOut (Display display, ResponseInputStream in) {
    super (display, in); 
  }
}
