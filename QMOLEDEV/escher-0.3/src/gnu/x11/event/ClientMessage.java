package gnu.x11.event;

import gnu.x11.Atom;
import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X client message event. */
public final class ClientMessage extends Event {
  public static final int CODE = 33;


  public int window_id;
  public int type_atom_id;
  public byte[] data;

  /** Reading. */
  public ClientMessage (Display display, ResponseInputStream in) {
    super (display, in); 
    window_id = in.read_int32 ();
    type_atom_id = in.read_int32 ();
    data = new byte[20];
    in.read_data(data);
  }


  //-- reading

  public int format () {
    return detail;
  }

  public int type_id () {
    return type_atom_id;
  }

  public int wm_data () {
    return data [0] << 24 | data [1] << 16 | data [2] << 8 | data [3];
  }

  public int wm_time () {
    return data [4] << 24 | data [5] << 16 | data [6] << 8 | data [7];
  }


  public boolean delete_window () {
    Atom wm_protocols = (Atom) Atom.intern (display, "WM_PROTOCOLS");
    Atom wm_delete_window = (Atom) Atom.intern (display,
      "WM_DELETE_WINDOW");

    return format () == 32
      && type () == wm_protocols
      && wm_data () == wm_delete_window.id;
  }


  public Atom type () { 
    return (Atom) Atom.intern (display, type_id (), true); 
  }


}
