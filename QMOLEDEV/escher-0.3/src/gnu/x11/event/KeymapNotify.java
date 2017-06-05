package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X keymap notify event. */
public final class KeymapNotify extends Event {
  public static final int CODE = 11;

  public Display display;

  public int code;

  public byte[] keys;

  public KeymapNotify (Display display, ResponseInputStream in) {
    super ();
    this.display = display;
    code = in.read_int8 ();
    keys = new byte[31];
    in.read_data (keys);
  }

}
