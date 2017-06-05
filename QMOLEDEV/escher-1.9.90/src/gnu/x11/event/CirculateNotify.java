package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X circulate notify event. */
public final class CirculateNotify extends Event {

  private int eventWindowID;
  private int windowID;
  
  private Place place;

  public CirculateNotify (Display display, ResponseInputStream in) {
    super (display, in);
    eventWindowID = in.readInt32();
    windowID = in.readInt32();
    in.skip (4); // Unused.
    place = Place.getByCode(in.readInt8());
    in.skip (15);
  }

}
