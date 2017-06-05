package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X button press event. */
public final class ButtonPress extends Input {
  public static final int CODE = 4;


  public ButtonPress (Display display, ResponseInputStream in) {
    super (display, in); 
  }

  public ButtonPress (Display display) {
    super (display, CODE);
  }
}
