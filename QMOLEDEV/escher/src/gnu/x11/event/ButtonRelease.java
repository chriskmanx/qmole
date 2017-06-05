package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X button release event. */
public final class ButtonRelease extends Input {

  public ButtonRelease (Display display, ResponseInputStream in) {
    super (display, in); 
  }
}
