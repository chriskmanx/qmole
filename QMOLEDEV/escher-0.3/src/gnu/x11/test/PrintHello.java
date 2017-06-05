package gnu.x11.test;

import gnu.x11.GC;
import gnu.x11.Window;
import gnu.x11.extension.Print;


/**
 * Hello World for X Print Service Extension.
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/PrintHello.help">
 * help output</a>
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/PrintHello.ps">
 * printer output</a>
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/PrintHello.output">
 * text output</a>
 */
public class PrintHello extends gnu.x11.Application {
  private static final String FONT =
    "-*-*-*-*-*-*-*-180-300-300-*-*-iso8859-1";


  public PrintHello (String [] args) 
    throws gnu.x11.extension.NotFoundException {
    
    super (args);

    String printer_name = option.string ("printer", 
      "name of printer", "");

    about ("0.1", "print hello world",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    Print print = new Print (display);
    Print.Context context = print.create_context (printer_name);

    System.out.println (print);
    System.out.println (context);

    context.set_attributes (Print.Context.JOB_ATTRIBUTE_POOL,
      Print.Context.ATTRIBUTE_MERGE,
      "*job-name: Hello world for Xprint");
    context.set ();
    print.start_job (Print.SPOOL);

    Window root = context.screen ();
    GC gc = root.screen ().default_gc ();
    gc.set_font (new gnu.x11.Font (display, FONT));
    Window window = new Window (root, 100, 100, 100, 100);
    window.create ();

    print.start_page (window);
    window.map ();
    window.text (gc, 20, 30, "Hello World!");
    print.end_page ();

    print.end_job ();
    context.destroy ();
    display.close ();    
  }


  public static void main (String [] args) throws Exception { 
    new PrintHello (args);
  }
}
