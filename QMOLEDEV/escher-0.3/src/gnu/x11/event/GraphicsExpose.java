package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X graphics expose event. */
public final class GraphicsExpose extends Event {
  public static final int CODE = 13;


  public int drawable_id;

  public int x;
  public int y;
  public int width;
  public int height;

  public int minor_opcode;
  public int count;
  public int major_opcode;

  
  public GraphicsExpose (Display display, ResponseInputStream in) {

    super (display, in);
    drawable_id = in.read_int32 ();
    x = in.read_int16 ();
    y = in.read_int16 ();
    width = in.read_int16 ();
    height = in.read_int16 ();
    minor_opcode = in.read_int16 ();
    count = in.read_int16 ();
    major_opcode = in.read_int8 ();
    in.skip (11);

  }
}
