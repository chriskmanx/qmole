package gnu.x11.event;

import gnu.x11.Atom;
import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X property notify event. */
public final class PropertyNotify extends Event {
  public static final int CODE = 28;

  public static final int NEW_VALUE = 0;
  public static final int DELETED = 1;

  public int window_id;
  public int atom_id;
  public int time;
  public int state;

  public PropertyNotify (Display display, ResponseInputStream in) {
    super (display, in);
    window_id = in.read_int32 ();
    atom_id = in.read_int32 ();
    time = in.read_int32 ();
    state = in.read_int8 ();
    in.skip (15);
  }


  public Atom atom (Display display) { 
    return (Atom) Atom.intern (display, atom_id, true);
  }
}
