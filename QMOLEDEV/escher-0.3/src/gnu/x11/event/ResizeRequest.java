package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X resize request event. */
public final class ResizeRequest extends Event {
  public static final int CODE = 25;

  public int window_id;

  public int width;
  public int height;

  
  public ResizeRequest (Display display, ResponseInputStream in) {
    super (display, in);
    window_id = in.read_int32 ();
    width = in.read_int16 ();
    height = in.read_int16 ();
    in.skip (20);
  }


  //-- reading

  public int width () {
    return width;
  }

  public int height () {
    return height;
  }

  /**
   * Returns the window ID of the resize request.
   *
   * @return the window ID of the resize request
   */
  public int window_id () {
    return window_id;
  }
}
