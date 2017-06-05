package gnu.x11;

/**
 * Thrown when the client is not able to connect to the server.
 * 
 * @author Mario Torre <neugens@aicas.com>
 */
public class EscherUnsupportedScreenBitDepthException extends EscherException {

   /**
    * 
    */
   public EscherUnsupportedScreenBitDepthException()
   {
     super();
   }

   /**
    * @param message
    */
   public EscherUnsupportedScreenBitDepthException(String message)
   {
     super(message);
   }

   /**
    * @param cause
    */
   public EscherUnsupportedScreenBitDepthException(Throwable cause)
   {
     super(cause);
   }

   /**
    * @param message
    * @param cause
    */
   public EscherUnsupportedScreenBitDepthException(String message, Throwable cause)
   {
     super(message, cause);
   }
}
