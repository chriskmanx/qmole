package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;

/**
 * The base class for all X events.
 */
public abstract class Event {

  public enum EventMask {
      NO_EVENT_MASK(0),
      KEY_PRESS_MASK(1<<0),
      KEY_RELEASE_MASK(1<<1),
      BUTTON_PRESS_MASK(1<<2),
      BUTTON_RELEASE_MASK(1<<3),
      ENTER_WINDOW_MASK(1<<4),
      LEAVE_WINDOW_MASK(1<<5),
      POINTER_MOTION_MASK(1<<6),
      POINTER_MOTION_HINT_MASK(1<<7),
      BUTTON1_MOTION_MASK(1<<8),
      BUTTON2_MOTION_MASK(1<<9),
      BUTTON3_MOTION_MASK(1<<10),
      BUTTON4_MOTION_MASK(1<<11),
      BUTTON5_MOTION_MASK(1<<12),
      BUTTON_MOTION_MASK(1<<13),
      KEYMAP_STATE_MASK(1<<14),
      EXPOSURE_MASK(1<<15),
      VISIBILITY_CHANGE_MASK(1<<16),
      STRUCTURE_NOTIFY_MASK(1<<17),
      RESIZE_REDIRECT_MASK(1<<18),
      SUBSTRUCTURE_NOTIFY_MASK(1<<19),
      SUBSTRUCTURE_REDIRECT_MASK(1<<20),
      FOCUS_CHANGE_MASK(1<<21),
      PROPERTY_CHANGE_MASK(1<<22),
      COLORMAP_CHANGE_MASK(1<<23),
      OWNER_GRAB_BUTTON_MASK(1<<24),
      LAST_MASK_INDEX(24);
      
      private int mask;
      
      EventMask(int mask) {
          this.mask = mask;
      }
      
      public int getMask() {
        return mask;
      }
      
      public static int maskOr(EventMask[] eventMasks) {
         int resultMask = 0;
         
         for (EventMask m : eventMasks) {
             resultMask = resultMask | m.getMask();
         }
         
         return resultMask;
      }
  }
  
  /**
   * The display from which this event originated.
   */
  public Display display;

  /**
   * The event code;
   */
  public EventCode code;

  /**
   * Event-specific detail information.
   */
  public int detail;

  /**
   * The sequence number of the event.
   */
  public int sequenceNumber;

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
    code = EventCode.getEventByID(in.readInt8());
    detail = in.readInt8 ();
    sequenceNumber = in.readInt16();
  }


  public Event (Display disp, EventCode c) {
    display = disp;
    code = c;
  }

  public EventCode code () {
    return code;
  }

  public int sequenceNumber() {
    return sequenceNumber;
  }

  public String toString() {
    String class_name = "#" + getClass ().getName ();
    return class_name + " " + code ();
  }

  /**
   * Writes this event into a request. This is used in
   * {@link gnu.x11.Window#sendEvent(boolean, int, Event)}.
   *
   * @param o the output stream to write to
   */
  public void write (RequestOutputStream o) {
    o.writeInt8(code.getCode());
    o.writeInt8(detail);
    o.writeInt16(sequenceNumber); // Is this correct?

    // The remaining pieces must be written by the subclasses.
  }

}
