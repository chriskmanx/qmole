package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X mapping notify event. */
public final class MappingNotify extends Event {

  private int request;
  private int firstKeycode;
  private int count;

  public MappingNotify (Display display, ResponseInputStream in) {
    super (display, in);
    request = in.readInt8();
    firstKeycode = in.readInt8();
    count = in.readInt8();
    in.skip(25);
  }
}
