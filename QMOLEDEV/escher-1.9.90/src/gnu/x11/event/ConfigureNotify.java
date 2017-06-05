package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.Rectangle;
import gnu.x11.ResponseInputStream;


/** X configure notify event. */
public final class ConfigureNotify extends Event {

  private int eventWindowID;
  private int windowID;
  private int aboveSiblingID;

  private int x;
  private int y;
  private int width;
  private int height;

  private int borderWidth;
  private boolean overrideRedirect;

  /** Reading. */
  public ConfigureNotify (Display display, ResponseInputStream in) {
    super(display, in);
    eventWindowID = in.readInt32();
    windowID = in.readInt32();
    aboveSiblingID = in.readInt32();
    x = in.readInt16();
    y = in.readInt16();
    width = in.readInt16();
    height = in.readInt16();
    borderWidth = in.readInt16();
    overrideRedirect = in.readBool();
    in.skip (5);
  }


  //-- reading

  public int eventID () {
    return eventWindowID;
  }

  public int aboveSiblingID () {
    return aboveSiblingID;
  }

  public int x () {
    return x;
  }

  public int y () {
    return y;
  }

  public int width () {
    return width;
  }

  public int height () {
    return height;
  }

  public Rectangle rectangle () {
    return new Rectangle (x (), y (), width (), height ());
  }

}
