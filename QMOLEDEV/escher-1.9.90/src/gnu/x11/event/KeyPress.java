package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/**
 * X key press event.
 */
public final class KeyPress extends Input {

  public KeyPress (Display display) {
    super (display, EventCode.KEY_PRESS);
  }

  /**
   * Reads a KeyPress event from the input stream.
   *
   * @param display the display from which this event originated.
   * @param in the input stream to read from
   */
  public KeyPress (Display display, ResponseInputStream in) {
    super (display, in);
  }

}
