package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;
import gnu.x11.Window;


/**
 * X input-related event.
 */
public abstract class Input extends Event {

  public int time;

  public int root_window_id;

  public int event_window_id;

  public int child_window_id;

  public int root_x;

  public int root_y;

  public int event_x;

  public int event_y;

  public int state;

  public boolean same_screen;

  /**
   * Reads the event from the input stream.
   */
  public Input (Display display, ResponseInputStream in) {
    
    super (display, in);
    time = in.read_int32 ();
    root_window_id = in.read_int32 ();
    event_window_id = in.read_int32 ();
    child_window_id = in.read_int32 ();
    root_x = in.read_int16 ();
    root_y = in.read_int16 ();
    event_x = in.read_int16 ();
    event_y = in.read_int16 ();
    state = in.read_int16 ();
    same_screen = in.read_bool ();
    in.skip (1); // Unused.
  }


  public Input (Display display, int code) {
    super (display, code);
  }


  public int detail () {
    return detail;
  }

  public int root_id () {
    return root_window_id;
  }

  public int child_id () {
    return child_window_id;
  }

  public int root_x () {
    return root_x;
  }

  public int root_y () {
    return root_y;
  }

  public int event_x () {
    return event_x;
  }

  public int event_y () {
    return event_y;
  }

  public int state () {
    return state;
  }

  public boolean same_screen () {
    return same_screen;
  }

  public Window root () { 
    return (Window) Window.intern (display, root_window_id); 
  }

  public Window child () {
    return (Window) Window.intern (display, child_window_id); 
  }

  public void set_window (Window w) {
    event_window_id = w.id;
  }

  public void set_detail (int d) {
    detail = d;
  }

  public void set_state (int s) {
    state = s;
  }

  public void write (RequestOutputStream o) {
    super.write (o);
    o.write_int32 (time);
    o.write_int32 (root_window_id);
    o.write_int32 (event_window_id);
    o.write_int32 (child_window_id);
    o.write_int16 (root_x);
    o.write_int16 (root_y);
    o.write_int16 (event_x);
    o.write_int16 (event_y);
    o.write_int16 (state);
    o.write_bool (same_screen);
    o.skip (1); // Unused.

  }
}
