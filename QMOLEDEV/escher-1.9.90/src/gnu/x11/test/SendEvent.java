package gnu.x11.test;

import gnu.x11.event.ButtonPress;
import gnu.x11.event.Event;
import gnu.x11.event.KeyPress;


/** 
 * Test sending synthetic events.
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/SendEvent.output">
 * text output</a>
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/SendEvent.help">
 * help output</a>
 */
public class SendEvent extends Graphics {
  public SendEvent (String [] args) {
    super (args, 100, 50);

    about ("0.1", "test sending synthetic events",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
  }


  public void exec () {
    if (help_option) return;

    System.out.println ("Sending a synthetic KeyPress...");
    KeyPress key_event = new KeyPress (display);
    key_event.set_window (window);
    key_event.set_detail (display.input.keysym_to_keycode ('t'));
    window.send_event (false, Event.NO_EVENT_MASK, key_event);
    
    System.out.println ("Sending a synthetic ButtonPress to exit...");
    ButtonPress button_event = new ButtonPress (display);
    key_event.set_window (window);
    window.send_event (false, Event.NO_EVENT_MASK, button_event);
    display.flush ();
    while (!exit_now) {
      dispatch_event ();
      System.out.println ("Received: " + event);
    }

    display.close ();
  }

    
  public static void main (String [] args) { 
    new SendEvent (args).exec ();
  }
}
