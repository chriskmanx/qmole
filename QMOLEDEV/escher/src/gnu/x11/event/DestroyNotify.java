package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X destroy notify event. */
public final class DestroyNotify extends Event {

  private int eventWindowID;
  private int windowID;

  public DestroyNotify (Display display, ResponseInputStream in) {
    super(display, in);
    eventWindowID = in.readInt32();
    windowID = in.readInt32();
    in.skip(20);
  }

  
  public int getEventWindowID() {
    return eventWindowID;
  }
}
