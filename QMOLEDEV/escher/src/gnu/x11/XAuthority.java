
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

  public enum EntryType {
      UNIX_IP_ADDRESS(0),
      UNIX_HOSTNAME(1),
      EOF(-1);
      
      private int code;
      
      EntryType(int code) {
          this.code = code;
      }

      public int getCode() {
          return this.code;
      }
      
      public static EntryType getByID(int id) {
          switch (id) {
              case 0: return UNIX_IP_ADDRESS;
              case 1: return UNIX_HOSTNAME;
              case -1: return EOF;
              default: return EOF;
          }
      }
  }

  /**
   * Stores the number of bytes read for the last read () call on the
   * InputStream from which we parse the data.
   */
  private static int bytesRead;

  /**
   * The type. Known types are
   */
  private EntryType type;

  /**
   * The hostname for the entry. This may be null, in which case you may
   * want to look at the ip_address entry.
   */
  private String hostname;

  /**
   * The IP address for the entry. This may be null, in which case you may
   * want to look at the hostname entry.
   */
  private byte[] ipAddress;

  /**
   * The display number.
   */
  private String displayNumber;

  /**
   * The protocol name.
   */
  private byte[] protocolName;

  /**
   * The actual protocol data.
   */
  private byte[] protocolData;

  /**
   * Reads one section of an Xauthority file, parses its contents and
   * initializes the fields accordingly.
   *
   * @param in the input stream from which to read the XAuthority section
   */
  private XAuthority (InputStream in) throws IOException {

    // The first byte seems to be the type of the entry. See the constant
    // fields in this class.
    type = EntryType.getByID(in.read());
    if (type == EntryType.EOF) {
      bytesRead = -1;
      return;
    }

    // The following two bytes seem to be always zero.
    in.read();
    in.read();

    // It follows the length of the hostname/ipadress entry and the actual
    // entry.
    byte[] data = readBlock(in);
    if (type == EntryType.UNIX_IP_ADDRESS) {
      ipAddress = new byte[4];
      System.arraycopy (data, 0, ipAddress, 0, 4);
    } else {
      hostname = new String (data);
    }

    in.read(); // Read one \0.

    // The following block is the port or something similar as string.
    data = readBlock(in);
    displayNumber = new String(data);

    in.read(); // Read one \0.

    // Now comes the protocol name.
    data = readBlock(in);
    protocolName = data;

    in.read(); // Read one \0.

    // Finally we read the actual data.
    data = readBlock(in);
    protocolData = data;
  }

  /**
   * Reads one block of data. The first byte read will specify the number
   * of bytes to be read, the following N bytes is the actual data, one more
   * byte is the trailing \0.
   * 
   * @return the actual data
   */
  private byte[] readBlock (InputStream in) throws IOException {
    int length = in.read();
    byte[] data = new byte[length];
    int index = 0;
    do {
      bytesRead = in.read(data, index, length - index);
      index += bytesRead;
    } while (length - index > 0);

    return data;    
  }

  /**
   * Fetches the current Xauthority entries from $HOME/.Xauthority or
   * whatever is specified in the environment variable $XAUTHORITY.
   *
   * @return the current Xauthority entries
   */
  public static XAuthority[] getAuthorities () {
    String authFilename = System.getenv ("XAUTHORITY");
    if (authFilename == null || authFilename.equals (""))
      authFilename = System.getProperty ("user.home") + File.separatorChar
                      + ".Xauthority";
    File auth_file = new File (authFilename);
    ArrayList authorities = new ArrayList ();
    FileInputStream in;
    try {
      in = new FileInputStream (auth_file);
      while (bytesRead != -1) {
        XAuthority xauth = new XAuthority (in);
        if (xauth.type != EntryType.EOF)
          authorities.add (xauth);
      }
      in.close ();
    } catch (FileNotFoundException ex) {
      System.err.println ("Can't find Xauthority file: " + authFilename);
    } catch (IOException ex) {
      System.err.println ("Unexpected problem while reading Xauthority file");
    }

    return (XAuthority[]) authorities.toArray
                                         (new XAuthority[authorities.size ()]);
  }

  public String toString () {
    StringBuilder s = new StringBuilder ();
    s.append ("hostname: " + hostname);
    s.append (", display: " + displayNumber);
    s.append (", protocol_name: " + new String (protocolName));
    return s.toString ();
  }
  
  
  public String getDisplayNumber() {

    return displayNumber;
  }


  public String getHostname() {

    return hostname;
  }


  public byte[] getProtocolData() {

    return protocolData;
  }

 
  public byte[] getProtocolName() {

    return protocolName;
  }
}
