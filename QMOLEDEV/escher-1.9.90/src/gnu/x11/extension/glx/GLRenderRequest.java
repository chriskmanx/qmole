package gnu.x11.extension.glx;

import gnu.x11.RequestObject;
import gnu.x11.RequestOutputStream;

/**
 * This encapsulates a GL render request buffer. This is used to enable
 * dynamic extension of the render request buffer.
 */
class GLRenderRequest implements RequestObject {

  /**
   * The default buffer size to start with.
   */
  static final int DEFAULT_BUFFER_SIZE = 256;

  /**
   * The request buffer.
   */
  private byte[] buffer;

  /**
   * The current index in the buffer.
   */
  private int index;

  /**
   * Creates a new instance of GLRenderRequest.
   */
  GLRenderRequest() {
    buffer = new byte[DEFAULT_BUFFER_SIZE];
  }

  void reset(int tag) {
    index = 0;
    writeInt32(tag);
  }

  /**
   * Writes the request to the specified output stream.
   *
   * @param s the output stream to write to
   */
  public void write(RequestOutputStream s) {
    // Update the length field.
    s.setIndex (2);
    int length = index / 4 + 1;
    s.writeInt16 (length);
    // And write it out.
    s.write(buffer, 0, index);
    //s.send ();
  }

  /**
   * Writes a 8 bit integer to the buffer.
   *
   * @param v the value to write
   */
  void writeInt8(int v) {
    buffer[index] = (byte) v;
    index++;
  }

  /**
   * Writes a 16 bit integer to the buffer.
   *
   * @param v the value to write
   */
  void writeInt16(int v) {
    buffer[index] = (byte) (v >> 8);
    index++;
    buffer[index] = (byte) v;
    index++;
  }

  /**
   * Writes a 16 bit integer to the buffer.
   *
   * @param v the value to write
   */
  void writeInt32(int v) {
    buffer[index] = (byte) (v >> 24);
    index++;
    buffer[index] = (byte) (v >> 16);
    index++;
    buffer[index] = (byte) (v >> 8);
    index++;
    buffer[index] = (byte) v;
    index++;
  }

  void writeFloat(float f) {

    int v = Float.floatToIntBits (f);

    buffer [index] = (byte) (v >> 24);
    index++;
    buffer [index] = (byte) (v >> 16);
    index++;
    buffer [index] = (byte) (v >> 8);
    index++;
    buffer [index] = (byte) v;
    index++;
  }

  void writeDouble(double d) {

    long v = Double.doubleToLongBits (d);
    
    buffer [index] = (byte) (v >> 56);
    index++;
    buffer [index] = (byte) (v >> 48);
    index++;
    buffer [index] = (byte) (v >> 40);
    index++;
    buffer [index] = (byte) (v >> 32);
    index++;
    buffer [index] = (byte) (v >> 24);
    index++;
    buffer [index] = (byte) (v >> 16);
    index++;
    buffer [index] = (byte) (v >> 8);
    index++;
    buffer [index] = (byte) v;
    index++;
  }

  public void writeBool(boolean b) {

    int v = (b ? 1 : 0);
    
    buffer [index] = (byte) (v);
    index++;   
  }
  
  public void writePad(int n) {
      
    index += RequestOutputStream.pad(n);
  }
 
  /**
   * Returns <code>true</code> when a request with the specified
   * <code>length</code> would fit into this render request, <code>false</code>
   * otherwise.
   *
   * @param length the required request length
   *
   * @return <code>true</code> when the request would fit, <code>false</code>
   *         otherwise
   */
  boolean fits(int length) {
    boolean fits = (buffer.length - length) > index;
    return fits;
  }
}
