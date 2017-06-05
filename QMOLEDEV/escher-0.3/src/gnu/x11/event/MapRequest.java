package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X map request event. */
public final class MapRequest extends Event {
  public static final int CODE = 20;

  public int parent_window_id;
  public int window_id;

  public MapRequest (Display display, ResponseInputStream in) {
    super (display, in);
    parent_window_id = in.read_int32 ();
    window_id = in.read_int32 ();
    in.skip (20);
  }
}
