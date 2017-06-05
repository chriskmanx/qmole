package gnu.x11.extension.glx;


/** GLX Error. */
public class Error extends gnu.x11.Error {
  public static final int BAD_CONTEXT = 0;
  public static final int BAD_CONTEXT_STATE = 1;
  public static final int BAD_DRAWABLE = 2;
  public static final int BAD_PIXMAP = 3;
  public static final int BAD_CONTEXT_TAG = 4;
  public static final int BAD_CURRENT_WINDOW = 5;
  public static final int BAD_RENDER_REQUEST = 6;
  public static final int BAD_LARGE_REQUEST = 7;
  public static final int BAD_PRIVATE_REQUEST = 8;
  public static final int BAD_FB_CONFIG = 9;
  public static final int BAD_PBUFFER = 10;
  public static final int BAD_CURRENT_DRAWABLE = 11;
  public static final int BAD_WINDOW = 12;


  public static final String [] ERROR_STRINGS = {
    "BAD_GLX_CONTEXT: parameter not a GLX rendering context",
    "BAD_GLX_CONTEXT_STATE: state of GLX rendering context inconsistent",
    "BAD_GLX_DRAWABLE: parameter not a GLX drawable",
    "BAD_GLX_PIXMAP: parameter not a GLX pixmap",
    "BAD_GLX_CONTEXT_TAG: tag of GLX rendering context invalid",
    "BAD_GLX_CURRENT_WINDOW: current GLX window invalid",
    "BAD_GLX_RENDER_REQUEST: parameter of GLX rendering request invalid",
    "BAD_GLX_LARGE_REQUEST: series of GLX large requests"
    + "incomplete or invalid",
    "BAD_GLX_PRIVATE_REQUEST: bad GLX private request code",
    "BAD_GLX_FB_CONFIG: parameter not a GLX frame-buffer config",
    "BAD_GLX_PBUFFER: parameter not a GLX pbuffer",
    "BAD_GLX_CURRENT_DRAWABLE: current GLX drawable invalid",
    "BAD_GLX_WINDOW: parameter not a GLX window"
  };


  public Error (gnu.x11.Display display, int code, int seq_no, int bad,
                int minor_opcode, int major_opcode) {
    
    super (display, ERROR_STRINGS [code], code, seq_no, bad, 
      minor_opcode, major_opcode); 
  }
}
