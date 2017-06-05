package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X motion notify event. */
public final class MotionNotify extends Input {

  public MotionNotify (Display display, ResponseInputStream in) {
    super(display, in);
  }

}
