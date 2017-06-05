package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;

public abstract class FocusEvent extends Event {

  public static final int NORMAL = 0;
  public static final int GRAB = 1;
  public static final int UNGRAB = 2;
  public static final int WHILE_GRABBED = 3;

  public int event_window_id;

  public int mode;

  public FocusEvent (Display display, ResponseInputStream in) {
    super (display, in);
    event_window_id = in.read_int32 ();
    mode = in.read_int8 ();
    in.skip (23); // Unused.
  }
}
