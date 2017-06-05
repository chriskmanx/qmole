package gnu.x11.extension;

import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;


/**
 * Big Requests Extension. The specification can be found <a href= 
 * "http://escher.sourceforge.net/etc/specification/bigrequest.ps.gz"
 * >here</a>.
 */
public class BigRequests extends Extension {
  public BigRequests (gnu.x11.Display display) throws NotFoundException { 
    super (display, "BIG-REQUESTS", null); 
  }


  // bigrequests opcode 0 - big req enable
  /**
   * @see <a href="XExtendedMaxRequestSize.html">
   * XExtendedMaxRequestSize</a>
   */
  public int enable () {
    int st;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 0, 1);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        st = i.read_int32 ();
        i.skip (20);
      }
    }
    return st;
  }
}
