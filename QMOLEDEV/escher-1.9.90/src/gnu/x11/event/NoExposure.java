package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X no exposure event. */
public final class NoExposure extends Event {

  private int drawableID;
  private int minorOpcode;
  private int majorOpcode;
  
  public NoExposure (Display display, ResponseInputStream in) {
    super(display, in);
    drawableID = in.readInt32();
    minorOpcode = in.readInt16();
    majorOpcode = in.readInt8();
    in.skip(21);
  }
}
