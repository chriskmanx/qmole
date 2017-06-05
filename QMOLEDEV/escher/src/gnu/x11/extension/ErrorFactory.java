package gnu.x11.extension;


public interface ErrorFactory {
  gnu.x11.Error build (gnu.x11.Display display, int code, int seqNumber, int bad,
                       int minorOpcode, int majorOpcode);
}
