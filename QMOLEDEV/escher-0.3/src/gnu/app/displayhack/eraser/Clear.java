package gnu.app.displayhack.eraser;


/** Simply clear. */
public class Clear extends Eraser {
  public Clear () { super ("clear"); }


  public void erase (gnu.app.displayhack.DisplayHack hack) {
    hack.window.clear (false);
    hack.sleep (hack.delay/2);  // before next screen
  }
}
