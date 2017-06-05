
package gnu.x11;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;

/**
 * A utility class to work with Xauthority files.
 *
 * This is reverse engineered by looking at one .Xauthority file.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class XAuthority {

  public static final int TYPE_UNIX_IP_ADDRESS = 0;
  public static final int TYPE_UNIX_HOSTNAME = 1;

  private static final int TYPE_EOF = -1;

  /**
   * Stores the number of bytes read for the last read () call on the
   * InputStream from which we parse the data.
   */
  private static int bytes_read;

  /**
   * The type. Known types are
   */
  public int type;

  /**
   * The hostname for the entry. This may be null, in which case you may
   * want to look at the ip_address entry.
   */
  public String hostname;

  /**
   * The IP address for the entry. This may be null, in which case you may
   * want to look at the hostname entry.
   */
  public byte[] ip_address;

  /**
   * The display number.
   */
  public String display;

  /**
   * The protocol name.
   */
  public byte[] protocol_name;

  /**
   * The actual protocol data.
   */
  public byte[] protocol_data;

  /**
   * Reads one section of an Xauthority file, parses its contents and
   * initializes the fields accordingly.
   *
   * @param in the input stream from which to read the XAuthority section
   */
  private XAuthority (InputStream in) throws IOException {

    // The first byte seems to be the type of the entry. See the constant
    // fields in this class.
    type = in.read ();
    if (type == TYPE_EOF) {
      bytes_read = -1;
      return;
    }

    // The following two bytes seem to be always zero.
    in.read ();
    in.read ();

    // It follows the length of the hostname/ipadress entry and the actual
    // entry.
    byte[] data = read_block (in);
    if (type == TYPE_UNIX_IP_ADDRESS) {
      ip_address = new byte[4];
      System.arraycopy (data, 0, ip_address, 0, 4);
    } else {
      hostname = new String (data);
    }

    in.read (); // Read one \0.

    // The following block is the port or something similar as string.
    data = read_block (in);
    display = new String (data);

    in.read (); // Read one \0.

    // Now comes the protocol name.
    data = read_block (in);
    protocol_name = data;

    in.read (); // Read one \0.

    // Finally we read the actual data.
    data = read_block (in);
    protocol_data = data;
  }

  /**
   * Reads one block of data. The first byte read will specify the number
   * of bytes to be read, the following N bytes is the actual data, one more
   * byte is the trailing \0.
   * 
   * @return the actual data
   */
  private byte[] read_block (InputStream in) throws IOException {
    int length = in.read ();
    byte[] data = new byte[length];
    int index = 0;
    do {
      bytes_read = in.read (data, index, length - index);
      index += bytes_read;
    } while (length - index > 0);

    return data;    
  }

  /**
   * Fetches the current Xauthority entries from $HOME/.Xauthority or
   * whatever is specified in the environment variable $XAUTHORITY.
   *
   * @return the current Xauthority entries
   */
  public static XAuthority[] get_authorities () {
    String auth_filename = System.getenv ("XAUTHORITY");
    if (auth_filename == null || auth_filename.equals (""))
      auth_filename = System.getProperty ("user.home") + File.separatorChar
                      + ".Xauthority";
    File auth_file = new File (auth_filename);
    ArrayList authorities = new ArrayList ();
    FileInputStream in;
    try {
      in = new FileInputStream (auth_file);
      while (bytes_read != -1) {
        XAuthority xauth = new XAuthority (in);
        if (xauth.type != TYPE_EOF)
          authorities.add (xauth);
      }
      in.close ();
    } catch (FileNotFoundException ex) {
      System.err.println ("Can't find Xauthority file: " + auth_filename);
    } catch (IOException ex) {
      System.err.println ("unexpected problem while reading Xauthority file");
    }

    return (XAuthority[]) authorities.toArray
                                         (new XAuthority[authorities.size ()]);
  }

  public String toString () {
    StringBuilder s = new StringBuilder ();
    s.append ("hostname: " + hostname);
    s.append (", display: " + display);
    s.append (", protocol_name: " + new String (protocol_name));
    return s.toString ();
  }
}
