package gnu.x11.test;

import gnu.x11.Window;
import gnu.x11.event.ClientMessage;
import gnu.x11.event.Expose;
import gnu.x11.event.Event;
import gnu.x11.event.KeyPress;


/** Base class for testing basic drawing. */
public abstract class Graphics extends gnu.x11.Application {  
  public Event event;
  public boolean leave_display_open;
  public Window window;


  public Graphics (String [] args, int width, int height) {
    super (args);

    Window.Attributes win_attr = new Window.Attributes ();
    win_attr.set_background (display.default_white);
    win_attr.set_border (display.default_black);
    win_attr.set_event_mask (Event.BUTTON_PRESS_MASK
      | Event.EXPOSURE_MASK | Event.KEY_PRESS_MASK);
    window = new Window (display.default_root, 10, 10, width, height,
                         5, win_attr);

    window.set_wm (this, "main");
    window.set_wm_delete_window ();
  }


  protected void paint () {}

  
  protected void about (String version, String description,
    String author, String url) {
    
    about (version, description, author, url,
      "\nTo quit, press 'q', 'Q', ESCAPE, or any button.");
  }


  protected void exec () {
    if (help_option) return;

    window.map ();
    display.flush ();
    while (!exit_now) dispatch_event ();
    if (!leave_display_open) display.close ();
  }


  protected void dispatch_event () {
    event = display.next_event ();

    switch (event.code ()) {
    case gnu.x11.event.ButtonPress.CODE:
      exit ();
      break;

    case ClientMessage.CODE:
      if (((ClientMessage) event).delete_window ()) exit ();
      break;

    case Expose.CODE:
      if (((Expose) event).count () == 0) paint ();
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
}
