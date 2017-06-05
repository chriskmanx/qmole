package gnu.x11.test;

import gnu.x11.Cursor;
import gnu.x11.extension.XCMisc;


/**
 * Test internal workings of <code>gnu.x11</code>. 
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Internal.output">
 * text output</a>
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Internal.help">
 * help output</a>
 */
public class Internal extends gnu.x11.Application {
  public Internal (String [] args) { 
    super (args);

    about ("0.1", "test internal workings of the library",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    // xc-misc, overflow it first
    display.resource_index = 0xfffffff0;
    new Cursor (display, 0);
    System.out.println ("xc-misc allocation test passed");


    // keyboard mapping
    int keycode = display.input.keysym_to_keycode (gnu.x11.keysym.Misc.DELETE);
    System.out.println ("keycode for DELETE: " + keycode);
    int keysym = display.input.keycode_to_keysym (keycode, 0);
    System.out.println ("keysym for " + keycode + ": " + keysym);
    if (keysym != gnu.x11.keysym.Misc.DELETE) throw new Error ();
    if (display.input.keysyms_per_keycode != 2)
      System.out.println ("WARNING: keysyms-per-keycode > 2: "
        + display.input.keysyms_per_keycode);
  }


  public static void main (String [] args) { 
    new Internal (args);
  }
}
