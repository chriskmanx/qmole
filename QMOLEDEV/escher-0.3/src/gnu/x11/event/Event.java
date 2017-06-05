package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;

/**
 * The base class for all X events.
 */
public abstract class Event {

  public static final int NO_EVENT_MASK = 0;
  public static final int KEY_PRESS_MASK = 1<<0;
  public static final int KEY_RELEASE_MASK = 1<<1;
  public static final int BUTTON_PRESS_MASK = 1<<2;
  public static final int BUTTON_RELEASE_MASK = 1<<3;
  public static final int ENTER_WINDOW_MASK = 1<<4;
  public static final int LEAVE_WINDOW_MASK = 1<<5;
  public static final int POINTER_MOTION_MASK = 1<<6;
  public static final int POINTER_MOTION_HINT_MASK = 1<<7;
  public static final int BUTTON1_MOTION_MASK = 1<<8;
  public static final int BUTTON2_MOTION_MASK = 1<<9;
  public static final int BUTTON3_MOTION_MASK = 1<<10;
  public static final int BUTTON4_MOTION_MASK = 1<<11;
  public static final int BUTTON5_MOTION_MASK = 1<<12;
  public static final int BUTTON_MOTION_MASK = 1<<13;
  public static final int KEYMAP_STATE_MASK = 1<<14;
  public static final int EXPOSURE_MASK = 1<<15;
  public static final int VISIBILITY_CHANGE_MASK = 1<<16;
  public static final int STRUCTURE_NOTIFY_MASK = 1<<17;
  public static final int RESIZE_REDIRECT_MASK = 1<<18;
  public static final int SUBSTRUCTURE_NOTIFY_MASK = 1<<19;
  public static final int SUBSTRUCTURE_REDIRECT_MASK = 1<<20;
  public static final int FOCUS_CHANGE_MASK = 1<<21;
  public static final int PROPERTY_CHANGE_MASK = 1<<22;
  public static final int COLORMAP_CHANGE_MASK = 1<<23;
  public static final int OWNER_GRAB_BUTTON_MASK = 1<<24;
  public static final int LAST_MASK_INDEX = 24;

  /**
   * The display from which this event originated.
   */
  public Display display;

  /**
   * The event code;
   */
  public int code;

  /**
   * Event-specific detail information.
   */
  public int detail;

  /**
   * The sequence number of the event.
   */
  public int sequence_number;

  /**
   * Creates an event without reading. This is used in subclasses that
   * don't use the usual first 3 fields.
   */
  Event () {
    // Nothing to do here.
  }

  /**
   * Reads the event from the input stream.
   */
  public Event (Display display, ResponseInputStream in) {
    this.display = display;
    code = in.read_int8 ();
    detail = in.read_int8 ();
    sequence_number = in.read_int16();
  }


  public Event (Display disp, int c) {
    display = disp;
    code = c;
  }

  public int code () {
    return code;
  }

  public int seq_no () {
    return sequence_number;
  }

  public String toString () {
    String class_name = "#" + getClass ().getName ();
    return class_name + " " + code ();
  }

  /**
   * Writes this event into a request. This is used in
   * {@link gnu.x11.Window#send_event(boolean, int, Event)}.
   *
   * @param o the output stream to write to
   */
  public void write (RequestOutputStream o) {
    o.write_int8 (code);
    o.write_int8 (detail);
    o.write_int16 (sequence_number); // Is this correct?

    // The remaining pieces must be written by the subclasses.
  }

}
