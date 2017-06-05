package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X create notify event. */
public final class CreateNotify extends Event {
  public static final int CODE = 16;


  public int parent_id;
  public int window_id;
  public int x;
  public int y;
  public int width;
  public int height;
  public int border_width;
  public boolean override_redirect;

  public CreateNotify (Display display, ResponseInputStream in) {
    super (display, in); 
    parent_id = in.read_int32 ();
    window_id = in.read_int32 ();
    x = in.read_int16 ();
    y = in.read_int16 ();
    width = in.read_int16 ();
    height = in.read_int16 ();
    border_width = in.read_int16 ();
    override_redirect = in.read_bool ();
    in.skip (9);
  }

}
