package gnu.app.displayhack.eraser;

import gnu.app.displayhack.DisplayHack;


/** Base class for erasers. */
public abstract class Eraser {
  public static final Eraser [] ALL = {
    new Circle (),
    new Clear (),
    new SlideLines (),
    new ThreeCircles ()
  };


  public static final int RANDOM_ERASER_INDEX = ALL.length;
  public static final String [] ALL_STRINGS = new String [ALL.length+1];
  static { 
    for (int i=0; i<ALL.length; i++) 
      ALL_STRINGS [i] = ALL [i].id;
    ALL_STRINGS [RANDOM_ERASER_INDEX] = "random";
  }


  public String id;

  
  public Eraser (String id) { this.id = id; }
  public abstract void erase (DisplayHack hack);


  public boolean sleep (DisplayHack hack) {
    boolean interrupted = hack.sleep (hack.eraser_delay);
    if (interrupted) hack.window.clear (false);
    return interrupted;
  }
}
