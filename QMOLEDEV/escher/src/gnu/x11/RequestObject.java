package gnu.x11;

/**
 * Objects that implement this interface can be written to a connection
 * as part of a request.
 */
public interface RequestObject {

  /**
   * Writes this request object to the specified connection.
   *
   * @param c the connection to write to
   */
  void write (RequestOutputStream c);
}
