/**
 * 
 */


package gnu.x11;

/**
 * Super class of all exceptions in Escher. This class is used to represent a
 * generic exception allowing users to not clutter the code with a long chain of
 * checked exceptions.
 * 
 * @author Mario Torre <neugens@aicas.com>
 */
public class EscherException
    extends Exception
{
  /**
   * 
   */
  public EscherException()
  {

    super();
  }

  /**
   * @param message
   */
  public EscherException(String message)
  {
    super(message);
  }

  /**
   * @param cause
   */
  public EscherException(Throwable cause)
  {
    super(cause);
  }

  /**
   * @param message
   * @param cause
   */
  public EscherException(String message, Throwable cause)
  {
    super(message, cause);
  }
}
