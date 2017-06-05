package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X circulate notify event. */
public final class CirculateNotify extends Event {
  public static final int CODE = 26;

  public static final int TOP = 0;
  public static final int BOTTOM = 1;

  public int event_window_id;
  public int window_id;
  
  public int place;

  public CirculateNotify (Display display, ResponseInputStream in) {
    super (display, in);
    event_window_id = in.read_int32 ();
    window_id = in.read_int32 ();
    in.skip (4); // Unused.
    place = in.read_int8 ();
    in.skip (15);
  }

}
