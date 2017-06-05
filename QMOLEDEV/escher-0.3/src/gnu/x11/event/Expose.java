package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X expose event. */
public final class Expose extends Event {
  public static final int CODE = 12;

  public int window_id;

  public int x;
  public int y;
  public int width;
  public int height;

  public int count;

  public Expose (Display display, ResponseInputStream in) {
    super (display, in);
    window_id = in.read_int32 ();
    x = in.read_int16 ();
    y = in.read_int16 ();
    width = in.read_int16 ();
    height = in.read_int16 ();
    count = in.read_int16 ();
    in.skip (14); // Unused.
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

  public int count () {
    return count;
  }

}
