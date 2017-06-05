package gnu.x11.test;

import gnu.x11.event.ClientMessage;
import gnu.x11.event.Expose;
import gnu.x11.event.KeyPress;
import java.util.Random;


/**
 * Test multi-threading and synchronization. 
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Sync.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Sync.output">
 * text output</a>
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Sync.help">
 * help output</a>
 */
public class Sync extends Graphics implements Runnable {
  public static final Random random = new Random ();
  public Thread thread = new Thread (this, "paint");


  public Sync (String [] args) { 
    super (args, 256, 256);

    about ("0.1", "test multi-threading and synchronization",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
    leave_display_open = true;
    
    System.out.println ("stress test with send-mode = round-trip");
  }


  
  public void dispatch_event () {
    System.out.println ("blocking-read event");
    event = display.next_event ();
    System.out.println ("got event " + event);

    switch (event.code ()) {
    case gnu.x11.event.ButtonPress.CODE:
      exit ();
      break;

    case ClientMessage.CODE:
      if (((ClientMessage) event).delete_window ()) exit ();
      break;

    case Expose.CODE:
      if (!thread.isAlive ()) thread.start ();
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


  protected void exit () {
    super.exit ();
    thread.interrupt ();
  }    


  public void run () {
    while (!exit_now) {
      // generate round-trip request during `Display#next_event()'
      System.out.println ("try round-trip request");
      display.input.input_focus ();
      System.out.println ("done round-trip request");

      // generate one-way request during `Display#next_event()'
      window.line (display.default_gc, 
        random.nextInt (window.width), random.nextInt (window.height),
        random.nextInt (window.width), random.nextInt (window.height));
      display.flush ();

      if (!exit_now) gnu.util.Misc.sleep (1000);
    }

    display.close ();
  }


  public static void main (String [] args) { 
    new Sync (args).exec ();
  }
}
