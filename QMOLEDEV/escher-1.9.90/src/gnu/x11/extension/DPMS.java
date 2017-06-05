package gnu.x11.extension;

import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;


/**
 * Display Power Management Signaling Extension. The specification can be
 * found <a href=
 * "http://escher.sourceforge.net/etc/specification/dpms-library.ps.gz"
 * >here</a> (<a href=
 * "http://escher.sourceforge.net/etc/specification/dpms-protocol.ps.gz"
 * >protocol</a>).
 * 
 */
public class DPMS extends Extension {
  private static final String[] MINOR_OPCODE_STRINGS = {
    "GetVersion",               // 0
    "Capable",                  // 1
    "GetTimeouts",              // 2
    "SetTimeouts",              // 3
    "Enable",                   // 4
    "Disable",                  // 5
    "ForceLevel",               // 6
    "Info"                      // 7
  };


  public static final int CLIENT_MAJOR_VERSION = 1;
  public static final int CLIENT_MINOR_VERSION = 1;


  private int serverMajorVersion, serverMinorVersion;


  // dpms opcode 0 - get version
  /**
   * @see <a href="DPMSQueryExtension.html">DPMSQueryExtension</a>
   */
  public DPMS(gnu.x11.Display display) throws NotFoundException {  
    super(display, "DPMS", MINOR_OPCODE_STRINGS); 

    // check version before any other operations
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 0, 2);
      o.writeInt16(CLIENT_MAJOR_VERSION);
      o.writeInt16(CLIENT_MINOR_VERSION);

      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.readReply(o);
        i.skip(8);
        serverMajorVersion = i.readInt16();
        serverMinorVersion = i.readInt16();
        i.skip(20);
      }
    }
  }
  
  
  // dpms opcode 1 - capable
  /**
   * Determine whether or not the currently running server's devices are
   * capable of DPMS operations. There is a <a href=
   * "../../../../etc/dpms-bug">bug</a> in all servers based on X
   * Consortium sample implementation up to R6.5 (including XFree86 4.0.1
   * or earlier): the sequence number of the reply is incorrect, causing a
   * "reply out of order" error.
   *
   * @see <a href="DPMSCapable.html">DPMSCapable</a>
   */
  public boolean capable() {
    boolean capable;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 1, 1);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.skip(8);
        capable = i.readBool();
        i.skip(23);
      }
    }
    return capable;
  }

  
  /** Reply of {@link #getTimeouts()} */
  public static class TimeoutsInfo {

    private int standby;
    private int suspend;
    private int off;
    TimeoutsInfo(ResponseInputStream i) {
      standby = i.readInt16();
      suspend = i.readInt16();
      off = i.readInt16();
    }

    public String toString() {
      return "#TimeoutsReply"
        + "\n  standby: " + standby
        + "\n  suspend: " + suspend
        + "\n  off: " + off;
    }
  }
  
  
  // dpms opcode 2 - get timeouts
  /**
   * @see <a href="DPMSGetTimeouts.html">DPMSGetTimeouts</a>
   */
  public TimeoutsInfo getTimeouts() {
    TimeoutsInfo info;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 2, 1);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.skip(8);
        info = new TimeoutsInfo(i);
        i.skip(18);
      }
    }
    return info;
  }


  // dpms opcode 3 - set timeouts
  /**
   * @see <a href="DPMSSetTimeouts.html">DPMSSetTimeouts</a>
   */
  public void setTimeouts(int standby, int suspend, int off) {

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 3, 3);
      o.writeInt16(standby);
      o.writeInt16(suspend);
      o.writeInt16(off);
      o.skip(2);
      o.send();
    }
  }


  // dpms opcode 4 - enable
  /**
   * @see <a href="DPMSEnable.html">DPMSEnable</a>
   */
  public void enable() {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 4, 1);
      o.send();
    }
  }


  // dpms opcode 5 - disable
  /**
   * @see <a href="DPMSDisable.html">DPMSDisable</a>
   */
  public void disable() {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 5, 1);
      o.send();
    }
  }

  /** DPMS Level */
  public enum Level {
    ON(0) {@Override public String toString() { return "on"; }},
    STAND_BY(1) {@Override public String toString() { return "stand-by"; }},
    SUSPEND(2) {@Override public String toString() { return "suspend"; }},
    OFF(3) {@Override public String toString() { return "off"; }};
   
    private int code;
    
    Level(int code) {
        this.code = code;
    }
    
    public abstract String toString();
    
    public int getCode() {
        return this.code;
    }
    
    public static Level getByID(int id) {
        switch (id) {
            case 0: return ON;
            case 1: return STAND_BY;
            case 2: return SUSPEND;
            case 3: return OFF;
            default: return ON;
        }
    }
  }

  // dpms opcode 6 - force level
  /**
   * @param level valid:
   * {@link Level},
   *
   * @see <a href="DPMSForceLevel.html">DPMSForceLevel</a>
   */
  public void forceLevel(Level level) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 6, 2);
      o.writeInt16(level.getCode());
      o.send();
    }
  }

  /** Reply of {@link #info()} */
  public static class Info {

    /**
     * One of: {@link Level}
     */
    private Level powerLevel;
    boolean state;

    Info(ResponseInputStream i) {
      powerLevel = Level.getByID(i.readInt16());
      state = i.readBool();
    } 

    public String toString() {
      return "#InfoReply"
        + "\n  state: " + state
        + "\n  level: " + powerLevel.toString();
    }
  }


  // dpms opcode 7 - info
  /**
   * @see <a href="DPMSInfo.html">DPMSInfo</a>
   */
  public Info info() {
    Info info;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 7, 1);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.readReply(o);
        i.skip(8);
        info = new Info(i);
        i.skip(21);
      }
    }
    return info;
  }


  public String moreString() {
    return "\n  client-version: " 
      + CLIENT_MAJOR_VERSION + "." + CLIENT_MINOR_VERSION
      + "\n  server-version: "
      + serverMajorVersion + "." + serverMinorVersion;
  }
}
