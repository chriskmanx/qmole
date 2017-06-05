package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X reparent notify event. */
public final class ReparentNotify extends Event {
  public static final int CODE = 21;


  public int event_window_id;
  public int window_id;
  public int parent_window_id;
  public int x;
  public int y;
  public boolean override_redirect;

  public ReparentNotify (Display display, ResponseInputStream in) {
    super (display, in);
    event_window_id = in.read_int32 ();
    window_id = in.read_int32 ();
    parent_window_id = in.read_int32 ();
    x = in.read_int16 ();
    y = in.read_int16 ();
    override_redirect = in.read_bool ();
    in.skip (11);
  }
}
