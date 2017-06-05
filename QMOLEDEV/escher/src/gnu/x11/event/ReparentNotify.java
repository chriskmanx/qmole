package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X reparent notify event. */
public final class ReparentNotify extends Event {

  private int eventWindowID;
  private int windowID;
  private int parentWindowID;
  private int x;
  private int y;
  private boolean overrideRedirect;

  public ReparentNotify (Display display, ResponseInputStream in) {
    super(display, in);
    eventWindowID = in.readInt32();
    windowID = in.readInt32();
    parentWindowID = in.readInt32 ();
    x = in.readInt16 ();
    y = in.readInt16 ();
    overrideRedirect = in.readBool ();
    in.skip (11);
  }
}
