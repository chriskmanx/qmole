package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.Rectangle;
import gnu.x11.ResponseInputStream;


/** X configure notify event. */
public final class ConfigureNotify extends Event {
  public static final int CODE = 22;

  public int event_window_id;
  public int window_id;
  public int above_sibling_id;

  public int x;
  public int y;
  public int width;
  public int height;

  public int border_width;
  public boolean override_redirect;

  /** Reading. */
  public ConfigureNotify (Display display, ResponseInputStream in) {
    super (display, in);
    event_window_id = in.read_int32 ();
    window_id = in.read_int32 ();
    above_sibling_id = in.read_int32 ();
    x = in.read_int16 ();
    y = in.read_int16 ();
    width = in.read_int16 ();
    height = in.read_int16 ();
    border_width = in.read_int16 ();
    override_redirect = in.read_bool();
    in.skip (5);
  }


  //-- reading

  public int event_id () {
    return event_window_id;
  }

  public int above_sibling_id () {
    return above_sibling_id;
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
