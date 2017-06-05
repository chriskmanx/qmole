package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/**
 * X key press event.
 */
public final class KeyPress extends Input {

  public static final int CODE = 2;

  public KeyPress (Display display) {
    super (display, CODE);
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
