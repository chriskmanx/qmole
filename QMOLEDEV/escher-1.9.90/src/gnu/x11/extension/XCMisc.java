package gnu.x11.extension;

import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;

/** 
 * XC-MISC Extension. The specification can be found <a href= 
 * "http://escher.sourceforge.net/etc/specification/xcmisc.ps.gz"
 * >here</a>.
 */
public class XCMisc extends Extension {
  public static final String[] MINOR_OPCODE_STRINGS = {
    "GetVersion",               // 0
    "GetXIDRange",              // 1
    "GetXIDList"                // 2
  };


  public static final int CLIENT_MAJOR_VERSION = 1;
  public static final int CLIENT_MINOR_VERSION = 1;

  private int serverMajorVersion, serverMinorVersion;


  // xc-misc opcode 0 - get version
  public XCMisc(gnu.x11.Display display) throws NotFoundException {
    super(display, "XC-MISC", MINOR_OPCODE_STRINGS); 

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

  
  /** Reply of {@link XCMisc#xidRange()} */
  public static class XIDRange {
    public int startID;
    public int count;
    XIDRange(ResponseInputStream i) {
      startID = i.readInt32();
      count = i.readInt32();
    }
  }
  
  
  // xc-misc opcode 1 - get xid range
  public XIDRange xidRange() {
    XIDRange r;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 1, 1);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.readReply(o);
        i.skip(8);
        r = new XIDRange(i);
        i.skip(16);
      }
    }
    return r;
  }


  // xc-misc opcode 2 - get xid list
  public int[] xidList(int count) {

    int [] ids;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 2, 2);
      
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.readReply(o);
        i.skip(8);
        int num = i.readInt32();
        i.skip(20);
        ids = new int [num];
        for (int resourceID : ids)
          resourceID = i.readInt32();
      }
    }
    return ids;
  }


  public String moreString() {
    return "\n  client-version: " 
      + CLIENT_MAJOR_VERSION + "." + CLIENT_MINOR_VERSION
      + "\n  server-version: "
      + serverMajorVersion + "." + serverMinorVersion;
  }
}
