package gnu.x11.test;


/** 
 * Hello World. Based on {@link Graphics}.
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Hello2.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Hello2.help">
 * help output</a>
 * 
 * @see Hello
 */
public class Hello2 extends Graphics {
  public Hello2 (String [] args) {
    super (args, 100, 50);

    about ("0.1", "hello world",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
  }


  public void paint () {
    window.text (display.default_gc, 20, 30, "Hello World!");
    display.flush ();
  }


  public static void main (String [] args) { 
    new Hello2 (args).exec ();
  }
}
