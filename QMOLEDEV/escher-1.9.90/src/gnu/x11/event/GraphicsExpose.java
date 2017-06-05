package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X graphics expose event. */
public final class GraphicsExpose extends Event {
  public static final int CODE = 13;


  private int drawableID;

  private int x;
  private int y;
  private int width;
  private int height;

  private int minorOpcode;
  private int count;
  private int majorOpcode;

  
  public GraphicsExpose (Display display, ResponseInputStream in) {

    super (display, in);
    drawableID = in.readInt32 ();
    x = in.readInt16();
    y = in.readInt16();
    width = in.readInt16();
    height = in.readInt16();
    minorOpcode = in.readInt16();
    count = in.readInt16();
    majorOpcode = in.readInt8();
    in.skip(11);

  }
}
