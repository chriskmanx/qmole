package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X visibility notify event. */
public final class VisibilityNotify extends Event {

  public enum State {
      UNOBSCURED(0),
      PARTIALLY_UNOBSCURED(1),
      FULLY_UNOBSCURED(2);
      
      private int code;
      
      State(int code) {
          this.code = code;
      }
      
      public static State getByCode(int code) {
          switch (code) {
              case 0: return UNOBSCURED;
              case 1: return PARTIALLY_UNOBSCURED;
              case 2: return FULLY_UNOBSCURED;
              default: return UNOBSCURED;
          }
      }
  }
  
  private int windowID;
  private State state;
  
  public VisibilityNotify (Display display, ResponseInputStream in) {
    super (display, in);
    windowID = in.readInt32();
    state = State.getByCode(in.readInt8());
    in.skip (23);
  }
}
