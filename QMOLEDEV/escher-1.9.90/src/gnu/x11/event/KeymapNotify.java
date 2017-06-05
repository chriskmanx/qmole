package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X keymap notify event. */
public final class KeymapNotify extends Event {

  private Display display;

  private int code;

  private byte[] keys;

  public KeymapNotify (Display display, ResponseInputStream in) {
    super ();
    this.display = display;
    code = in.readInt8();
    keys = new byte[31];
    in.readData(keys);
  }

}
