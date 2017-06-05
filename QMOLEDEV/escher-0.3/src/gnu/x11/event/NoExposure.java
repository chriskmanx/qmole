package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X no exposure event. */
public final class NoExposure extends Event {
  public static final int CODE = 14;

  public int drawable_id;
  public int minor_opcode;
  public int major_opcode;
  
  public NoExposure (Display display, ResponseInputStream in) {
    super (display, in);
    drawable_id = in.read_int32 ();
    minor_opcode = in.read_int16();
    major_opcode = in.read_int8 ();
    in.skip (21);
  }
}
