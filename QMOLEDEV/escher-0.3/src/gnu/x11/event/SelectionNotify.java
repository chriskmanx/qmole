package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X selection notify event. */
public final class SelectionNotify extends Event {
  public static final int CODE = 31;


  public int time;
  public int requestor_window_id;
  public int selection_atom_id;
  public int target_atom_id;
  public int property_atom_id;

  public SelectionNotify (Display display, ResponseInputStream in) {
    super (display, in);
    time = in.read_int32 ();
    requestor_window_id = in.read_int32 ();
    selection_atom_id = in.read_int32 ();
    target_atom_id = in.read_int32();
    property_atom_id = in.read_int32 ();
    in.skip (8);
  }
}
