package gnu.x11.event;

import gnu.x11.Atom;
import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X property notify event. */
public final class PropertyNotify extends Event {

  public enum State {
      NEW_VALUE(0),
      DELETED(1);
      
      private int code;
      
      State(int code) {
          this.code = code;
      }
      
      public int getCode() {
          return code;
      }
      
      public static State getByCode(int code) {
          return code == 0 ? NEW_VALUE : DELETED;
      }
  }

  private int windowID;
  private int atomID;
  private int time;
  private State state;

  public PropertyNotify (Display display, ResponseInputStream in) {
    super(display, in);
    windowID = in.readInt32();
    atomID = in.readInt32();
    time = in.readInt32();
    state = State.getByCode(in.readInt8());
    in.skip(15);
  }

  /**
   * @deprecated use {@link #getAtom(Display)} instead.
   * @param display
   * @return
   */
  @Deprecated
  public Atom atom (Display display) { 
    return (Atom) Atom.intern (display, atomID, true);
  }
  
  public Atom getAtom(Display display) {
      
      return atom(display);
  }
  
  public int getAtomID() {
      
      return this.atomID;
  }
  
  public int getWindowID() {
      
      return this.windowID;
  }
  
  public int getTime() {

      return this.time;
  }
  
  public State getState() {
      
      return this.state;
  }
}
