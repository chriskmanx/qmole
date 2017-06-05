package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;

public abstract class FocusEvent extends Event {

  public enum Mode {
      NORMAL(0),
      GRAB(1),
      UNGRAB(2),
      WHILE_GRABBED(3);
      
      private int code;
      
      Mode(int code) {
          this.code = code;
      }
      
      public static Mode getByCode(int code) {
          switch (code) {
              case 0: return NORMAL;
              case 1: return GRAB;
              case 2: return UNGRAB;
              case 3: return WHILE_GRABBED;
              default: return NORMAL;
          }
      }
      
  }

  private int eventWindowID;

  private Mode mode;

  public FocusEvent (Display display, ResponseInputStream in) {
    super (display, in);
    eventWindowID = in.readInt32();
    mode = Mode.getByCode(in.readInt8());
    in.skip(23); // Unused.
  }

    public int getEventWindowID() {
        // Should really return the Window object here.
        return eventWindowID;
    }
}
