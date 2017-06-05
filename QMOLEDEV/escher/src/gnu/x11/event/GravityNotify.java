package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X gravity notify event. */
public final class GravityNotify extends Event {

  private int eventWindowID;
  private int windowID;

  private int x;
  private int y;

  
  public GravityNotify (Display display, ResponseInputStream in) {
    super (display, in);
    eventWindowID = in.readInt32();
    windowID = in.readInt32();
    x = in.readInt16();
    y = in.readInt16();
    in.skip(16);
  }
}
