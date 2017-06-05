package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X mapping notify event. */
public final class MappingNotify extends Event {
  public static final int CODE = 34;

  public int request;
  public int first_keycode;
  public int count;

  public MappingNotify (Display display, ResponseInputStream in) {
    super (display, in);
    request = in.read_int8 ();
    first_keycode = in.read_int8 ();
    count = in.read_int8 ();
    in.skip (25);
  }
}
