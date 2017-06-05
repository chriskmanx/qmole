package gnu.x11.test;

import gnu.x11.Window;
import gnu.x11.event.ClientMessage;
import gnu.x11.event.Expose;
import gnu.x11.event.Event;
import gnu.x11.event.KeyPress;


/** 
 * Hello World.
 *
 * <p>This program covers the basic elements of a primitive X
 * application. It intensionally does not base on {@link Graphics}.
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Hello.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Hello.help">
 * help output</a>
 * 
 * @see Hello2
 */
public class Hello extends gnu.x11.Application {
  public Hello (String [] args) {
    super (args);

    about ("0.1", "hello world",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo quit, press 'q', 'Q', ESCAPE, or any button.");
   
    if (help_option) return;

    Window.Attributes win_attr = new Window.Attributes ();
    win_attr.set_background (display.default_white);
    win_attr.set_border (display.default_black);
    win_attr.set_event_mask (Event.BUTTON_PRESS_MASK
      | Event.EXPOSURE_MASK | Event.KEY_PRESS_MASK);
    Window window = new Window (display.default_root, 10, 10,
      100, 50, 5, win_attr);
    
    window.set_wm (this, "main");
    window.set_wm_delete_window ();
    window.map ();
    display.flush ();

    while (!exit_now) {
      Event event = display.next_event ();

      switch (event.code ()) {
      case gnu.x11.event.ButtonPress.CODE:
        exit ();
        break;

      case ClientMessage.CODE:
        if (((ClientMessage) event).delete_window ()) exit ();
        break;

      case Expose.CODE:
        if (((Expose) event).count () == 0) {
          window.text (display.default_gc, 20, 30, "Hello World!");
          display.flush ();
        }
        break;
	
      case KeyPress.CODE: {
        KeyPress e = (KeyPress) event;
	
        int keycode = e.detail ();
        int keystate = e.state ();
        int keysym = display.input.keycode_to_keysym (keycode, keystate);

        if (keysym == 'q' || keysym == 'Q' 
          || keysym == gnu.x11.keysym.Misc.ESCAPE) exit ();
        break;
      }      
      }
    }

    display.close ();
  }


  public static void main (String [] args) {
    new Hello (args);
  }
}
