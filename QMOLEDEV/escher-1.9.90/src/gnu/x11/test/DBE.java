package gnu.x11.test;

import java.util.Random;


/**
 * Test Double Buffer Extension. 
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/DBE.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/DBE.output">
 * text output</a>
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/DBE.help">
 * help output</a>
 */
public class DBE extends Graphics {
  public gnu.x11.extension.DBE.BackBuffer back_buffer;
  public gnu.x11.extension.DBE dbe;
  public static final Random random = new Random ();


  public DBE (String [] args) throws gnu.x11.extension.NotFoundException {
    super (args, 256, 256); 
    
    about ("0.1", "test double buffer extension",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    dbe = new gnu.x11.extension.DBE (display);

    // server bug
    //System.out.println (dbe.visual_info (gnu.x11.extension.DBE.EMPTY)
    //  .toString (Enum.NEXT, "\n"));    

    // test allocation and deallocation
    back_buffer = dbe.allocate (window, gnu.x11.extension.DBE.UNDEFINED);
    back_buffer.deallocate ();

    // test extension error
    try {
      back_buffer = dbe.allocate (window, gnu.x11.extension.DBE.UNDEFINED);
      back_buffer.id = 666;
      back_buffer.deallocate ();
      display.check_error ();

    } catch (Error e) {
      System.out.println ("Forced error for testing: " + e);
    }

    // test get back buffer attributes
    back_buffer = dbe.allocate (window, gnu.x11.extension.DBE.UNDEFINED);
    System.out.println ("back buffer attributes: " 
      + back_buffer.attributes ());
  }


  public void paint () {   
    for (int i=0; i<10; i++) {
      back_buffer.line (display.default_gc, 
        random.nextInt (back_buffer.width), 
        random.nextInt (back_buffer.height),
        random.nextInt (back_buffer.width), 
        random.nextInt (back_buffer.height));
    }

    back_buffer.swap (gnu.x11.extension.DBE.UNDEFINED);
    display.flush ();
  }


  public static void main (String [] args) throws Exception { 
    new DBE (args).exec ();
  }
}
