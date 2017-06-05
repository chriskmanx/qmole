package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X key release event. */
public final class KeyRelease extends Input {

  public static final int CODE = 3;


  /**
   * Reads a KeyRelease event from the input stream.
   *
   * @param display the display from which this event originated
   * @param in the input stream
   */
  public KeyRelease (Display display, ResponseInputStream in) {
    super (display, in);
  }

}
