package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X destroy notify event. */
public final class DestroyNotify extends Event {
  public static final int CODE = 17;

  public int event_window_id;
  public int window_id;

  public DestroyNotify (Display display, ResponseInputStream in) {
    super (display, in);
    event_window_id = in.read_int32 ();
    window_id = in.read_int32 ();
    in.skip (20);
  }

}
