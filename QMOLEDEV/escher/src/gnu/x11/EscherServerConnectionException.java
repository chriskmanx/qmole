package gnu.x11;

/**
 * Thrown when the client is not able to connect to the server.
 * 
 * @author Mario Torre <neugens@aicas.com>
 */
public class EscherServerConnectionException
    extends EscherException
{
  /**
   * 
   */
  public EscherServerConnectionException()
  {
    super();
  }

  /**
   * @param message
   */
  public EscherServerConnectionException(String message)
  {
    super(message);
  }

  /**
   * @param cause
   */
  public EscherServerConnectionException(Throwable cause)
  {
    super(cause);
  }

  /**
   * @param message
   * @param cause
   */
  public EscherServerConnectionException(String message, Throwable cause)
  {
    super(message, cause);
  }
}
