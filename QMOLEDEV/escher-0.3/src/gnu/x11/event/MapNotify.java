package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X map notify event. */
public final class MapNotify extends Event {
  public static final int CODE = 19;

  public int event_window_id;
  public int window_id;
  public boolean override_redirect;

  public MapNotify (Display display, ResponseInputStream in) {
    super (display, in);
    event_window_id = in.read_int32 ();
    window_id = in.read_int32 ();
    override_redirect = in.read_bool ();
    in.skip (19);
  }
}
