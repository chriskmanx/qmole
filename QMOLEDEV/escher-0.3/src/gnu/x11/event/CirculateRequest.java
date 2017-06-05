package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X circulate request event. */
public final class CirculateRequest extends Event {
  public static final int CODE = 27;

  public static final int TOP = 0;
  public static final int BOTTOM = 1;

  public int parent_window_id;
  public int window_id;
  public int place;

  public CirculateRequest (Display display, ResponseInputStream in) {
    super (display, in);
    parent_window_id = in.read_int32 ();
    window_id = in.read_int32 ();
    in.skip (4); // Unused.
    place = in.read_int8 ();
    in.skip (15);
  }

}
