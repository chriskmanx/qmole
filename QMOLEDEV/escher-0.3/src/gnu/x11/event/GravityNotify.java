package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X gravity notify event. */
public final class GravityNotify extends Event {
  public static final int CODE = 24;

  public int event_window_id;
  public int window_id;

  public int x;
  public int y;

  
  public GravityNotify (Display display, ResponseInputStream in) {
    super (display, in);
    event_window_id = in.read_int32 ();
    window_id = in.read_int32 ();
    x = in.read_int16 ();
    y = in.read_int16 ();
    in.skip (16);
  }
}
