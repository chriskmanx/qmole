package gnu.util;

import java.io.DataInputStream;


/** Unix environment. */
public class Environment {
  /**
   * All pairs of environment names and values. Prefixed with
   * <code>"\n"</code> to aid searching.
   */
  public static String ALL;


  static {
    try {        
      // TODO windows 95/nt: `command /c set'
      Process process = Runtime.getRuntime ().exec ("env");
      process.waitFor ();
      DataInputStream dis = new DataInputStream (
        process.getInputStream ());     
      int count = dis.available ();
      byte [] buffer = new byte [count];
      dis.readFully (buffer);

      // prefix with "\n" to aid searching
      ALL = "\n" + new String (buffer);

    } catch (Exception e) {
      ALL = "<failed to access system environment>";
    }
  }


  /** Get the value of an environment variable. */
  public static String value (String name) {
    /* A search key is prefixed with "\n" to avoid partial matching such as
     * matching `HOSTDISPLAY=canning-home:0.0' instead of `DISPLAY=:0.0'. 
     */
    String key = "\n" + name + "=";

    int pair_start = ALL.indexOf (key);
    if (pair_start == -1) return null;
    int value_start = pair_start + key.length ();
    int value_end = ALL.indexOf ('\n', value_start);
    return ALL.substring (value_start, value_end);
  }
}
