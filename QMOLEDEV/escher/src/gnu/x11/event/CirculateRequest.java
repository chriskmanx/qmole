package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X circulate request event. */
public final class CirculateRequest extends Event {

  private int parentWindowID;
  private int windowID;
  private int place;

  public CirculateRequest (Display display, ResponseInputStream in) {
    super (display, in);
    parentWindowID = in.readInt32();
    windowID = in.readInt32();
    in.skip(4); // Unused.
    place = in.readInt8();
    in.skip(15);
  }

}
